
{-# LANGUAGE OverloadedStrings #-}

module MOO.Connection (
    Connection
  , ConnectionHandler
  , connectionHandler

  , firstConnectionId

  , withConnections
  , withConnection
  , withMaybeConnection

  , connectionName
  , connectionConnectedTime
  , connectionActivityTime
  , connectionOutputDelimiters

  , notify'
  , notify

  , bufferedOutputLength
  , forceInput
  , flushInput
  , bootPlayer

  , setConnectionOption
  , getConnectionOptions
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM, TVar,
                               atomically, newTVar,
                               readTVar, writeTVar, modifyTVar)
import Control.Concurrent.STM.TBMQueue (TBMQueue, newTBMQueue, closeTBMQueue,
                                        readTBMQueue, writeTBMQueue,
                                        tryReadTBMQueue, tryWriteTBMQueue,
                                        unGetTBMQueue, isEmptyTBMQueue,
                                        freeSlotsTBMQueue)
import Control.Exception (SomeException, try, bracket)
import Control.Monad ((<=<), join, when, unless, foldM, forever, void)
import Control.Monad.Trans (lift)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (Decoding(..), encodeUtf8, streamDecodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (UTCTime, getCurrentTime)
import Pipes (Producer, Consumer, Pipe, await, yield, runEffect,
              for, cat, (>->))
import Pipes.Concurrent (Buffer(..), Output(..), spawn, spawn',
                         fromInput, toOutput)

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V

import MOO.Task
import MOO.Types

import qualified MOO.String as Str

data Connection = Connection {
    connectionPlayer           :: TVar ObjId

  , connectionInput            :: TBMQueue ConnectionMessage
  , connectionOutput           :: TBMQueue ConnectionMessage

  , connectionName             :: ConnectionName
  , connectionConnectedTime    :: TVar (Maybe UTCTime)
  , connectionActivityTime     :: TVar UTCTime

  , connectionOutputDelimiters :: TVar (Text, Text)
  , connectionOptions          :: TVar ConnectionOptions
  }

data ConnectionMessage = Line Text | Binary ByteString

data ConnectionOptions = ConnectionOptions {
    optionBinaryMode        :: Bool
  , optionHoldInput         :: Bool
  , optionDisableOOB        :: Bool
  , optionClientEcho        :: Bool
  , optionFlushCommand      :: Text
  , optionIntrinsicCommands :: Map Text IntrinsicCommand
  }

initConnectionOptions = ConnectionOptions {
    optionBinaryMode        = False
  , optionHoldInput         = False
  , optionDisableOOB        = False
  , optionClientEcho        = True
  , optionFlushCommand      = ".flush"
  , optionIntrinsicCommands = allIntrinsicCommands
  }

data ConnectionOption = Option {
    optionName :: Id
  , optionGet  :: ConnectionOptions -> Value
  , optionSet  :: Connection -> Value ->
                  ConnectionOptions -> MOO ConnectionOptions
  }

coBinary = Option "binary" get set
  where get = truthValue . optionBinaryMode
        set _ v options = return options { optionBinaryMode = truthOf v }

coHoldInput = Option "hold-input" get set
  where get = truthValue . optionHoldInput
        set _ v options = return options { optionHoldInput = truthOf v }

coDisableOOB = Option "disable-oob" get set
  where get = truthValue . optionDisableOOB
        set _ v options = return options { optionDisableOOB = truthOf v }

coClientEcho = Option "client-echo" get set
  where get = truthValue . optionClientEcho
        set conn v options = do
          let clientEcho = truthOf v
              telnetCommand = BS.pack [telnetIAC, telnetOption, telnetECHO]
              telnetOption  = if clientEcho then telnetWON'T else telnetWILL
          liftSTM $ enqueueOutput False conn (Binary telnetCommand)
          return options { optionClientEcho = clientEcho }

          where telnetIAC   = 255
                telnetWILL  = 251
                telnetWON'T = 252
                telnetECHO  = 1

coFlushCommand = Option "flush-command" get set
  where get = Str . Str.fromText . optionFlushCommand
        set _ v options = do
          let flushCommand = case v of
                Str flush -> Str.toText flush
                _         -> T.empty
          return options { optionFlushCommand = flushCommand }

coIntrinsicCommands = Option "intrinsic-commands" get set
  where get = fromListBy (Str . Str.fromText) . M.keys . optionIntrinsicCommands
        set _ v options = do
          commands <- case v of
            Lst vs -> foldM addCommand M.empty (V.toList vs)
            Int 0  -> return M.empty
            Int _  -> return allIntrinsicCommands
            _      -> raise E_INVARG

          return options { optionIntrinsicCommands = commands }

        addCommand cmds value = case value of
          Str cmd -> do
            ic <- maybe (raise E_INVARG) return $
                  M.lookup (Str.toText cmd) allIntrinsicCommands
            return $ M.insert (intrinsicCommand ic) ic cmds
          _ -> raise E_INVARG

allConnectionOptions :: Map Id ConnectionOption
allConnectionOptions = M.fromList $ map assoc connectionOptions
  where assoc co = (optionName co, co)
        connectionOptions = [
            coBinary
          , coHoldInput
          , coDisableOOB
          , coClientEcho
          , coFlushCommand
          , coIntrinsicCommands
          ]

data IntrinsicCommand = Intrinsic {
    intrinsicCommand       :: Text
  , intrinsicFunction      :: Connection -> Text -> IO ()
  }

modifyOutputDelimiters :: Connection -> ((Text, Text) -> (Text, Text)) -> IO ()
modifyOutputDelimiters conn =
  atomically . modifyTVar (connectionOutputDelimiters conn)

icPREFIX = Intrinsic "PREFIX" $ \conn prefix ->
  modifyOutputDelimiters conn $ \(_, suffix) -> (prefix, suffix)

icSUFFIX = Intrinsic "SUFFIX" $ \conn suffix ->
  modifyOutputDelimiters conn $ \(prefix, _) -> (prefix, suffix)

icOUTPUTPREFIX = icPREFIX { intrinsicCommand = "OUTPUTPREFIX" }
icOUTPUTSUFFIX = icSUFFIX { intrinsicCommand = "OUTPUTSUFFIX" }

icProgram = Intrinsic ".program" $ \conn argstr -> return ()  -- XXX

allIntrinsicCommands :: Map Text IntrinsicCommand
allIntrinsicCommands = M.fromList $ map assoc intrinsicCommands
  where assoc ic = (intrinsicCommand ic, ic)
        intrinsicCommands = [
            icPREFIX
          , icSUFFIX
          , icOUTPUTPREFIX
          , icOUTPUTSUFFIX
          , icProgram
          ]

outOfBandPrefix :: Text
outOfBandPrefix = "#$#"

outOfBandQuotePrefix :: Text
outOfBandQuotePrefix = "#$\""

maxQueueLength :: Int
maxQueueLength = 128

type ConnectionName = STM String
type ConnectionHandler = ConnectionName -> (Producer ByteString IO (),
                                            Consumer ByteString IO ()) -> IO ()

connectionHandler :: TVar World -> ObjId -> Bool -> ConnectionHandler
connectionHandler world' object printMessages connectionName (input, output) =
  bracket newConnection deleteConnection $ \conn -> do
    forkIO $ runConnection object printMessages conn

    forkIO $ do
      try (runEffect $ input >-> connectionRead conn) >>=
        either (\err -> let _ = err :: SomeException in return ()) return
      atomically $ closeTBMQueue (connectionOutput conn)

    runEffect $ connectionWrite conn >-> output
    atomically $ closeTBMQueue (connectionInput conn)

  where newConnection :: IO Connection
        newConnection = do
          now <- getCurrentTime

          atomically $ do
            world <- readTVar world'

            let connectionId = nextConnectionId world

            playerVar   <- newTVar connectionId

            inputQueue  <- newTBMQueue maxQueueLength
            outputQueue <- newTBMQueue maxQueueLength

            cTimeVar    <- newTVar Nothing
            aTimeVar    <- newTVar now

            delimsVar   <- newTVar ("", "")
            optionsVar  <- newTVar initConnectionOptions

            let connection = Connection {
                    connectionPlayer           = playerVar

                  , connectionInput            = inputQueue
                  , connectionOutput           = outputQueue

                  , connectionName             = connectionName
                  , connectionConnectedTime    = cTimeVar
                  , connectionActivityTime     = aTimeVar

                  , connectionOutputDelimiters = delimsVar
                  , connectionOptions          = optionsVar

                  }
                nextId = succConnectionId
                         (`M.member` connections world) connectionId

            writeTVar world' world {
                connections      = M.insert connectionId connection
                                   (connections world)
              , nextConnectionId = nextId
              }

            return connection

        deleteConnection :: Connection -> IO ()
        deleteConnection connection = atomically $ do
          connectionId <- readTVar (connectionPlayer connection)
          modifyTVar world' $ \world ->
            world { connections = M.delete connectionId (connections world) }

runConnection :: ObjId -> Bool -> Connection -> IO ()
runConnection object printMessages conn = echoLines

  where echoLines = do
          input <- atomically read
          case input of
            Just (Line line) -> do
              atomically (write $ Line line)
              echoLines
            Just (Binary _) -> echoLines
            Nothing -> return ()

        read  = readTBMQueue  (connectionInput  conn)
        write = writeTBMQueue (connectionOutput conn)

-- | Connection reading thread: deliver messages from the network to our input
-- 'TBMQueue' until EOF, handling binary mode and the flush command, if any.
connectionRead :: Connection -> Consumer ByteString IO ()
connectionRead conn = do
  (outputLines,    inputLines)    <- lift $ spawn Unbounded
  (outputMessages, inputMessages) <- lift $ spawn Unbounded

  lift $ forkIO $ runEffect $
    fromInput inputLines >->
    readUtf8 >-> readLines >-> sanitize >-> lineMessage >->
    toOutput outputMessages

  lift $ forkIO $ runEffect $ fromInput inputMessages >-> deliverMessages

  forever $ do
    bytes <- await

    lift $ getCurrentTime >>=
      atomically . writeTVar (connectionActivityTime conn)

    options <- lift $ atomically $ readTVar (connectionOptions conn)
    lift $ atomically $
      if optionBinaryMode options
      then send outputMessages (Binary bytes)
      else send outputLines bytes

  where readUtf8 :: Monad m => Pipe ByteString Text m ()
        readUtf8 = await >>= readUtf8' . streamDecodeUtf8With lenientDecode
          where readUtf8' (Some text _ continue) = do
                  unless (T.null text) $ yield text
                  await >>= readUtf8' . continue

        readLines :: Monad m => Pipe Text Text m ()
        readLines = readLines' TL.empty
          where readLines' prev = await >>= yieldLines prev >>= readLines'

                yieldLines :: Monad m => TL.Text -> Text ->
                              Pipe Text Text m TL.Text
                yieldLines prev text =
                  let concatenate = TL.append prev . TL.fromStrict
                      (end, rest) = T.break (== '\n') text
                      line  = TL.toStrict (concatenate end)
                      line' = if not (T.null line) && T.last line == '\r'
                              then T.init line else line
                  in if T.null rest
                     then return $ concatenate text
                     else do
                       yield line'
                       yieldLines TL.empty (T.tail rest)

        sanitize :: Monad m => Pipe Text Text m ()
        sanitize = for cat (yield . T.filter validStrChar)

        lineMessage :: Monad m => Pipe Text ConnectionMessage m ()
        lineMessage = for cat (yield . Line)

        deliverMessages :: Consumer ConnectionMessage IO ()
        deliverMessages = forever $ do
          message <- await
          case message of
            Line line -> do
              options <- lift $ atomically $ readTVar (connectionOptions conn)
              let flushCmd = optionFlushCommand options
              lift $ if not (T.null flushCmd) && line == flushCmd
                     then flushQueue else enqueue message
            Binary _ -> lift $ enqueue message

        enqueue :: ConnectionMessage -> IO ()
        enqueue = atomically . writeTBMQueue (connectionInput conn)

        flushQueue :: IO ()
        flushQueue = atomically $ flushInput True conn

-- | Connection writing thread: deliver messages from our output 'TBMQueue' to
-- the network until the queue is closed, which is our signal to close the
-- connection.
connectionWrite :: Connection -> Producer ByteString IO ()
connectionWrite conn = do
  (outputBinary, inputBinary, sealBinary) <- lift $ spawn' Unbounded
  (outputLines,  inputLines,  sealLines)  <- lift $ spawn' Unbounded

  lift $ forkIO $ do
    runEffect $ fromInput inputLines >-> writeLines >-> writeUtf8 >->
      toOutput outputBinary
    atomically sealBinary

  lift $ forkIO $ processQueue outputLines outputBinary sealLines

  fromInput inputBinary

  where writeLines :: Monad m => Pipe Text Text m ()
        writeLines = forever $ await >>= yield >> yield "\r\n"

        writeUtf8 :: Monad m => Pipe Text ByteString m ()
        writeUtf8 = for cat (yield . encodeUtf8)

        processQueue :: Output Text -> Output ByteString -> STM () -> IO ()
        processQueue outputLines outputBinary sealLines = loop
          where loop = do
                  message <- atomically $ readTBMQueue (connectionOutput conn)
                  case message of
                    Just (Line text) ->
                      atomically (send outputLines  text)  >> loop
                    Just (Binary bytes) ->
                      atomically (send outputBinary bytes) >> loop
                    Nothing -> atomically sealLines

-- | The first un-logged-in connection ID. Avoid conflicts with #-1
-- ($nothing), #-2 ($ambiguous_match), and #-3 ($failed_match).
firstConnectionId :: ObjId
firstConnectionId = -4

-- | Determine the next valid un-logged-in connection ID, making sure it
-- remains negative and doesn't pass the predicate.
succConnectionId :: (ObjId -> Bool) -> ObjId -> ObjId
succConnectionId invalid = findNext
  where findNext connId
          | nextId >= 0    = findNext firstConnectionId
          | invalid nextId = findNext nextId
          | otherwise      = nextId
          where nextId = connId - 1

-- | Do something with the 'Map' of all active connections.
withConnections :: (Map ObjId Connection -> MOO a) -> MOO a
withConnections f = f . connections =<< getWorld

-- | Do something with the connection for given object, if any.
withMaybeConnection :: ObjId -> (Maybe Connection -> MOO a) -> MOO a
withMaybeConnection oid f = withConnections $ f . M.lookup oid

-- | Do something with the connection for the given object, raising 'E_INVARG'
-- if no such connection exists.
withConnection :: ObjId -> (Connection -> MOO a) -> MOO a
withConnection oid = withMaybeConnection oid . maybe (raise E_INVARG)

enqueueOutput :: Bool -> Connection -> ConnectionMessage -> STM Bool
enqueueOutput noFlush conn message = do
  let queue = connectionOutput conn
  result <- tryWriteTBMQueue queue message
  case result of
    Just False -> if noFlush then return False
                  else do readTBMQueue queue  -- XXX count flushed lines
                          enqueueOutput noFlush conn message
    _          -> return True

-- | Send data to a connection, optionally without flushing.
notify' :: Bool -> ObjId -> StrT -> MOO Bool
notify' noFlush who what = do
  delayIO (putStrLn $ Str.toString what)  -- XXX

  withMaybeConnection who . maybe (return True) $ \conn -> do
    options <- liftSTM $ readTVar (connectionOptions conn)
    message <- if optionBinaryMode options
               then Binary `fmap` binaryString what
               else return (Line $ Str.toText what)
    liftSTM $ enqueueOutput noFlush conn message

-- | Send data to a connection, flushing if necessary.
notify :: ObjId -> StrT -> MOO Bool
notify = notify' False

-- | Return the number of items currently buffered for output to a connection,
-- or the maximum number of items that will be buffered up for output on any
-- connection.
bufferedOutputLength :: Maybe Connection -> STM Int
bufferedOutputLength Nothing     = return maxQueueLength
bufferedOutputLength (Just conn) =
  (maxQueueLength -) `fmap` freeSlotsTBMQueue (connectionOutput conn)

-- | Force a line of input for a connection.
forceInput :: Bool -> ObjId -> StrT -> MOO ()
forceInput atFront oid line =
  withConnection oid $ \conn -> do
    let queue   = connectionInput conn
        message = Line (Str.toText line)
    success <- liftSTM $
      if atFront then unGetTBMQueue queue message >> return True
      else fromMaybe True `fmap` tryWriteTBMQueue queue message
    unless success $ raise E_QUOTA

-- | Flush a connection's input queue, optionally showing what was flushed.
flushInput :: Bool -> Connection -> STM ()
flushInput showMessages conn = do
  let queue  = connectionInput conn
      notify = when showMessages . void . enqueueOutput False conn . Line
  empty <- isEmptyTBMQueue queue
  if empty then notify ">> No pending input to flush..."
    else do notify ">> Flushing the following pending input:"
            flushQueue queue notify
            notify ">> (Done flushing)"

    where flushQueue queue notify = loop
            where loop = do
                    item <- join `fmap` tryReadTBMQueue queue
                    case item of
                      Just (Line line) -> do
                        notify $ ">>     " <> line
                        loop
                      Just (Binary _)  -> do
                        notify   ">>   * [binary data]"
                        loop
                      Nothing -> return ()

-- | Close a connection.
bootPlayer :: ObjId -> MOO ()
bootPlayer oid = withMaybeConnection oid . maybe (return ()) $ \conn -> do
  liftSTM $ closeTBMQueue (connectionOutput conn)
  modifyWorld $ \world -> world { connections =
                                     M.delete oid (connections world) }
  -- XXX callSystemVerb "user_disconnected" [Obj oid]
  return ()

withConnectionOptions :: ObjId -> (ConnectionOptions -> MOO a) -> MOO a
withConnectionOptions oid f =
  withConnection oid $ f <=< liftSTM . readTVar . connectionOptions

modifyConnectionOptions :: ObjId -> (Connection -> ConnectionOptions ->
                                     MOO ConnectionOptions) -> MOO ()
modifyConnectionOptions oid f =
  withConnection oid $ \conn -> do
    let optionsVar = connectionOptions conn
    options <- liftSTM (readTVar optionsVar)
    liftSTM . writeTVar optionsVar =<< f conn options

-- | Set a connection option for an active connection.
setConnectionOption :: ObjId -> Id -> Value -> MOO ()
setConnectionOption oid option value =
  modifyConnectionOptions oid $ \conn options -> do
    option <- maybe (raise E_INVARG) return $
              M.lookup option allConnectionOptions
    optionSet option conn value options

-- | Return a 'Map' of all currently set connection options for a connection.
getConnectionOptions :: ObjId -> MOO (Map Id Value)
getConnectionOptions oid =
  withConnectionOptions oid $ \options ->
    let optionValue option = optionGet option options
    in return $ M.map optionValue allConnectionOptions
