
{-# LANGUAGE OverloadedStrings #-}

module MOO.Network ( Listener
                   , Connection
                   , PortNumber
                   , connectionEstablishedTime
                   , connectionActivityTime
                   , formatListener
                   , connectionOption
                   , setConnectionOption
                   , bootPlayer
                   , notify
                   , getConnectionName
                   , listen
                   , unlisten
                   , openNetworkConnection
                   ) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM (TVar, atomically,
                               readTVar, writeTVar, modifyTVar)
import Control.Exception (try, mask, finally)
import Control.Monad (when, foldM, forever, void)
import Network (Socket, HostName, PortID(PortNumber, UnixSocket), PortNumber,
                listenOn, accept, connectTo, socketPort, sClose)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import System.IO (Handle, BufferMode(LineBuffering), hClose, hSetBuffering)
import System.IO.Error (isPermissionError, isDoesNotExistError)

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

import {-# SOURCE #-} MOO.Database (systemObject, getServerOption')
import MOO.Task
import MOO.Types

import qualified MOO.String as Str

data Listener = Listener {
    listenerSocket        :: Socket
  , listenerThread        :: ThreadId

  , listenerPort          :: PortID
  , listenerObject        :: ObjId
  , listenerPrintMessages :: Bool
  }

initListener = Listener {
    listenerSocket        = undefined
  , listenerThread        = undefined

  , listenerPort          = PortNumber 0
  , listenerObject        = systemObject
  , listenerPrintMessages = True
  }

formatListener :: Listener -> Value
formatListener listener =
  fromList [ Obj $ listenerObject listener
           , formatPoint $ listenerPort listener
           , truthValue $ listenerPrintMessages listener
           ]

  where formatPoint (PortNumber port) = Int $ fromIntegral port
        formatPoint (UnixSocket path) = Str $ Str.fromString path

data Connection = Connection {
    connectionHandle                  :: Handle
  , connectionName                    :: Text

  , connectionEstablishedTime         :: Maybe UTCTime
  , connectionActivityTime            :: UTCTime

  , connectionOutputPrefix            :: Text
  , connectionOutputSuffix            :: Text

  , connectionOptionBinary            :: Bool
  , connectionOptionHoldInput         :: Bool
  , connectionOptionDisableOob        :: Bool
  , connectionOptionClientEcho        :: Bool
  , connectionOptionFlushCommand      :: Text
  , connectionOptionIntrinsicCommands :: Set IntrinsicCommand
  }

initConnection = Connection {
    connectionHandle                  = undefined
  , connectionName                    = T.empty

  , connectionEstablishedTime         = Nothing
  , connectionActivityTime            = undefined

  , connectionOutputPrefix            = T.empty
  , connectionOutputSuffix            = T.empty

  , connectionOptionBinary            = False
  , connectionOptionHoldInput         = False
  , connectionOptionDisableOob        = False
  , connectionOptionClientEcho        = True
  , connectionOptionFlushCommand      = ".flush"
  , connectionOptionIntrinsicCommands = allIntrinsicCommands
  }

data IntrinsicCommand = IntrinsicProgram
                      | IntrinsicPrefix
                      | IntrinsicSuffix
                      | IntrinsicOutputPrefix
                      | IntrinsicOutputSuffix
                      deriving (Enum, Bounded, Eq, Ord, Show)

ic2text :: IntrinsicCommand -> Text
ic2text IntrinsicProgram      = ".program"
ic2text IntrinsicPrefix       = "PREFIX"
ic2text IntrinsicSuffix       = "SUFFIX"
ic2text IntrinsicOutputPrefix = "OUTPUTPREFIX"
ic2text IntrinsicOutputSuffix = "OUTPUTSUFFIX"

text2ic :: Text -> Maybe IntrinsicCommand
text2ic str | T.toCaseFold str == ".program" = Just IntrinsicProgram
text2ic "PREFIX"       = Just IntrinsicPrefix
text2ic "SUFFIX"       = Just IntrinsicSuffix
text2ic "OUTPUTPREFIX" = Just IntrinsicOutputPrefix
text2ic "OUTPUTSUFFIX" = Just IntrinsicOutputSuffix
text2ic _              = Nothing

allIntrinsicCommands :: Set IntrinsicCommand
allIntrinsicCommands = S.fromList [minBound ..]

allConnectionOptions :: [Text]
allConnectionOptions = ["binary", "hold-input", "disable-oob", "client-echo",
                        "flush-command", "intrinsic-commands"]

connectionOption :: StrT -> Maybe (Connection -> Value)
connectionOption "binary"             =
  Just (truthValue . connectionOptionBinary)
connectionOption "hold-input"         =
  Just (truthValue . connectionOptionHoldInput)
connectionOption "disable-oob"        =
  Just (truthValue . connectionOptionDisableOob)
connectionOption "client-echo"        =
  Just (truthValue . connectionOptionClientEcho)
connectionOption "flush-command"      =
  Just (Str . Str.fromText . connectionOptionFlushCommand)
connectionOption "intrinsic-commands" =
  Just (fromListBy (Str . Str.fromText . ic2text) .
        S.toList . connectionOptionIntrinsicCommands)
connectionOption _                    = Nothing

setConnectionOption :: StrT -> Value -> Connection -> MOO Connection
setConnectionOption "binary" value conn =
  return conn { connectionOptionBinary = truthOf value }
setConnectionOption "hold-input" value conn =
  return conn { connectionOptionHoldInput = truthOf value }
setConnectionOption "disable-oob" value conn =
  return conn { connectionOptionDisableOob = truthOf value }
setConnectionOption "client-echo" value conn = do
  let clientEcho = truthOf value
      telnetCmd  = BS.pack [ telnetIAC
                           , if clientEcho then telnetWON'T else telnetWILL
                           , telnetECHO
                           ]
      telnetIAC   = 255
      telnetWILL  = 251
      telnetWON'T = 252
      telnetECHO  =   1
  delayIO $ BS.hPut (connectionHandle conn) telnetCmd
  return conn { connectionOptionClientEcho = clientEcho }
setConnectionOption "flush-command" value conn =
  return conn { connectionOptionFlushCommand = flushCommand }
  where flushCommand = case value of
          Str t -> Str.toText t
          _     -> T.empty
setConnectionOption "intrinsic-commands" value conn = do
  intrinsicCommands <- case value of
    Lst v -> foldM command S.empty (V.toList v)
      where command set (Str cmd) = maybe (raise E_INVARG)
                                    (return . flip S.insert set) $
                                    text2ic (Str.toText cmd)
            command _   _         = raise E_INVARG
    Int 0 -> return S.empty
    Int _ -> return allIntrinsicCommands
    _     -> raise E_INVARG
  return conn { connectionOptionIntrinsicCommands = intrinsicCommands }
setConnectionOption _ _ _ = raise E_INVARG

bootPlayer :: ObjId -> MOO ()
bootPlayer oid = notyet "bootPlayer"

notify :: ObjId -> StrT -> MOO ()
notify who what = delayIO (putStrLn $ Str.toString what)

getConnectionName :: ObjId -> MOO Text
getConnectionName player = do
  world <- getWorld
  case M.lookup player (connections world) of
    Just conn -> return $ connectionName conn
    Nothing   -> raise E_INVARG

listen :: PortNumber -> ObjId -> Bool -> MOO PortNumber
listen port object printMessages = do
  world <- getWorld
  when (M.member port $ listeners world) $ raise E_INVARG

  world' <- getWorld'
  result <- requestIO $ do
    result <- try $ listenOn (PortNumber port)
    case result of
      Left err
        | isPermissionError err -> return (Left E_PERM)
        | otherwise             -> return (Left E_QUOTA)
      Right socket -> do
        port <- socketPort socket
        let listener = initListener {
                listenerSocket        = socket
              , listenerPort          = port
              , listenerObject        = object
              , listenerPrintMessages = printMessages
              }
        thread <- forkIO $ listenerIO world' listener
        return $ Right listener { listenerThread = thread }

  case result of
    Left  err      -> raise err
    Right listener -> do
      let (PortNumber canon) = listenerPort listener
      putWorld world { listeners = M.insert canon listener (listeners world) }
      return canon

unlisten :: PortNumber -> MOO ()
unlisten port = do
  world <- getWorld
  case M.lookup port $ listeners world of
    Just Listener { listenerThread = thread, listenerSocket = socket } -> do
      putWorld world { listeners = M.delete port (listeners world) }
      delayIO $ killThread thread >> sClose socket
    Nothing -> raise E_INVARG

listenerIO :: TVar World -> Listener -> IO ()
listenerIO world' Listener {
    listenerSocket        = socket
  , listenerPort          = port
  , listenerObject        = object
  , listenerPrintMessages = printMessages
  } = mask $ \restore -> do

  let lport = case port of
        PortNumber port -> T.pack (show port)
        UnixSocket path -> T.pack (show path)

  forever $ do
    (handle, host, port) <- accept socket

    now <- getCurrentTime
    let connection = initConnection {
            connectionHandle       = handle
          , connectionName         = T.concat [ "port ", lport
                                              , " from ", T.pack host
                                              , ", port ", T.pack (show port)
                                              ]
          , connectionActivityTime = now
          }

    connId <- addConnection world' connection

    forkIO $
      restore (connectionIO world' object printMessages (connId, connection))
      `finally` (removeConnection world' connId >> hClose handle)

addConnection :: TVar World -> Connection -> IO ObjId
addConnection world' connection = atomically $ do
  world <- readTVar world'
  let connId = nextConnectionId world
  writeTVar world' world {
      connections = M.insert connId connection (connections world)
    , nextConnectionId = connId - 1
    }
  return connId

removeConnection :: TVar World -> ObjId -> IO ()
removeConnection world' connId = atomically $ modifyTVar world' $ \world ->
  world { connections = M.delete connId (connections world) }

openNetworkConnection :: HostName -> PortNumber -> ObjId -> MOO ObjId
openNetworkConnection host port object = do
  world' <- getWorld'
  result <- requestIO $ do
    result <- try $ connectTo host (PortNumber port)
    case result of
      Left err
        | isDoesNotExistError err -> return (Left E_INVARG)
        | otherwise               -> return (Left E_QUOTA)
      Right handle -> do
        now <- getCurrentTime
        let lport = "0"  -- XXX
            connection = initConnection {
                connectionHandle       = handle
              , connectionName         =
                T.concat [ "port ", lport
                         , " to ", T.pack host
                         , ", port ", T.pack (show port)
                         ]
              , connectionActivityTime = now
              }

        connId <- addConnection world' connection
        return $ Right (connId, connection)

  case result of
    Left  err -> raise err
    Right (connId, connection) -> do
      delayIO $ void $ forkIO $
        connectionIO world' object False (connId, connection)
        `finally` do
          removeConnection world' connId
          hClose $ connectionHandle connection

      return connId

connectionIO :: TVar World -> ObjId -> Bool -> (ObjId, Connection) -> IO ()
connectionIO world' object printMessages (connId, connection) = do
  let handle = connectionHandle connection
  hSetBuffering handle LineBuffering
  return ()

serverMessage :: ObjId -> Id -> [Text] -> MOO [Text]
serverMessage object name defaultMessage = do
  maybeValue <- getServerOption' object name
  undefined
