
{-# LANGUAGE OverloadedStrings #-}

module MOO.Connection (
    Connection
  , ConnectionHandler
  , connectionHandler

  , firstConnectionId
  , defaultConnectionEncoding

  , withConnections
  , withConnection

  , connectionName
  , connectionConnectedTime
  , connectionActivityTime
  , connectionOutputDelimiters

  , notify'
  , notify

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
                                        tryReadTBMQueue, tryWriteTBMQueue)

import Control.Exception (try, bracket, finally)
import Control.Monad ((<=<), join, unless, foldM)
import Data.ByteString (ByteString, hGetSome, hPut)
import Data.Map (Map)
import Data.Text (Text)
import Data.Text.IO (hGetLine, hPutStrLn)
import Data.Time (UTCTime, getCurrentTime)
import System.IO (Handle, TextEncoding, utf8,
                  hFlush, hClose, hIsEOF,
                  hSetEncoding, mkTextEncoding,
                  hSetNewlineMode, NewlineMode(..), Newline(..),
                  hSetBuffering, BufferMode(..))
import System.IO.Error (isDoesNotExistError)

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

import MOO.Task
import MOO.Types

import qualified MOO.String as Str

data Connection = Connection {
    connectionPlayer           :: TVar ObjId

  , connectionInput            :: TBMQueue InputMessage
  , connectionOutput           :: TBMQueue OutputMessage

  , connectionName             :: ConnectionName
  , connectionConnectedTime    :: TVar (Maybe UTCTime)
  , connectionActivityTime     :: TVar UTCTime

  , connectionOutputDelimiters :: TVar (Text, Text)
  , connectionOptions          :: TVar ConnectionOptions
  }

data InputMessage  = InputLine  Text | InputBinary  ByteString
data OutputMessage = OutputLine Text | OutputBinary ByteString | OutputFlush

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
          mapM_ (liftSTM . enqueueOutput False conn)
            [OutputBinary telnetCommand, OutputFlush]
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

binaryReadSize :: Int
binaryReadSize = 1024

type ConnectionName = STM String
type ConnectionHandler = ConnectionName -> Handle -> IO ()

connectionHandler :: TVar World -> ObjId -> Bool -> ConnectionHandler
connectionHandler world' object printMessages connectionName handle = do
  hSetBuffering handle LineBuffering  -- (BlockBuffering Nothing)
  hSetNewlineMode handle NewlineMode { inputNL = CRLF, outputNL = CRLF }

  encoding <- atomically $ connectionEncoding `fmap` readTVar world'
  hSetEncoding handle encoding  -- XXX is there a better approach?

  bracket newConnection deleteConnection $ \conn -> do
    forkIO $ connectionRead  handle conn
    forkIO $ connectionWrite handle conn

    runConnection object printMessages conn

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
runConnection object printMessages conn = do
  atomically read
  return ()

  where read  = readTBMQueue  (connectionInput  conn)
        write = writeTBMQueue (connectionOutput conn)

-- | Connection reading thread: deliver messages from the network to our input
-- 'TBMQueue' until EOF, handling binary mode and the flush command, if any.
connectionRead :: Handle -> Connection -> IO ()
connectionRead h conn = connectionRead' `finally` closeQueues
  where connectionRead' = do
          eof <- hIsEOF h
          unless eof $ do
            options <- atomically $ readTVar (connectionOptions conn)
            if optionBinaryMode options
              then enqueue =<< InputBinary `fmap` hGetSome h binaryReadSize
              else do line <- hGetLine h
                      let flushCmd = optionFlushCommand options
                      if not (T.null flushCmd) && line == flushCmd
                        then flushQueue
                        else enqueue (InputLine $ sanitize line)
            connectionRead'

        enqueue :: InputMessage -> IO ()
        enqueue = atomically . writeTBMQueue (connectionInput conn)

        flushQueue :: IO ()
        flushQueue = atomically flushQueue'
          where flushQueue' = do
                  item <- join `fmap` tryReadTBMQueue (connectionInput conn)
                  case item of
                    Just _  -> flushQueue'
                    Nothing -> return ()

        sanitize :: Text -> Text
        sanitize = T.filter validStrChar

        closeQueues = atomically $ do
          closeTBMQueue (connectionInput  conn)
          closeTBMQueue (connectionOutput conn)

-- | Connection writing thread: deliver messages from our output 'TBMQueue' to
-- the network until the queue is closed, which is our signal to close the
-- connection.
connectionWrite :: Handle -> Connection -> IO ()
connectionWrite h conn = connectionWrite'
  where connectionWrite' = do
          message <- atomically $ readTBMQueue (connectionOutput conn)
          case message of
            Just (OutputLine text)    -> hPutStrLn h text  >> connectionWrite'
            Just (OutputBinary bytes) -> hPut      h bytes >> connectionWrite'
            Just  OutputFlush         -> hFlush    h       >> connectionWrite'
            Nothing                   -> hClose    h

defaultConnectionEncoding :: IO TextEncoding
defaultConnectionEncoding = do
  result <- try $ mkTextEncoding "UTF-8//IGNORE"
  case result of
    Left err | isDoesNotExistError err || otherwise -> return utf8
    Right enc -> return enc

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

enqueueOutput :: Bool -> Connection -> OutputMessage -> STM Bool
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
               then OutputBinary `fmap` binaryString what
               else return (OutputLine $ Str.toText what)
    liftSTM $ enqueueOutput noFlush conn message

-- | Send data to a connection, flushing if necessary.
notify :: ObjId -> StrT -> MOO Bool
notify = notify' False

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
