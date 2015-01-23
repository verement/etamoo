
module MOO.Network.TCP (
    HostName
  , PortNumber
  , createTCPListener
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (STM, TMVar, newEmptyTMVarIO, atomically,
                               putTMVar, readTMVar)
import Control.Exception (SomeException, mask, try, finally, bracketOnError)
import Control.Monad (liftM, forever)
import Data.Maybe (fromMaybe)
import Network.Socket (PortNumber, Socket, SockAddr, SocketOption(..),
                       Family(AF_INET6), SocketType(Stream),
                       AddrInfo(..), AddrInfoFlag(..), NameInfoFlag(..),
                       HostName, ServiceName, maxListenQueue,
                       defaultHints, getAddrInfo, setSocketOption,
                       socket, bind, listen, accept, close,
                       getNameInfo, socketPort)
import Pipes.Network.TCP (fromSocket, toSocket)

import MOO.Connection (ConnectionHandler)
import {-# SOURCE #-} MOO.Network (Point(..), Listener(..))

maxBufferSize :: Int
maxBufferSize = 1024

serverAddrInfo :: Maybe HostName -> PortNumber -> IO [AddrInfo]
serverAddrInfo host port =
  let hints = defaultHints {
          addrFlags      = [AI_PASSIVE, AI_NUMERICSERV,
                            AI_ADDRCONFIG, AI_V4MAPPED]
        , addrFamily     = AF_INET6
        , addrSocketType = Stream
        }
  in getAddrInfo (Just hints) host (Just $ show port)

createTCPListener :: Listener -> ConnectionHandler -> IO Listener
createTCPListener listener handler = do
  let TCP host port = listenerPoint listener
  (ai:_) <- serverAddrInfo host port

  let mkSocket = socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai)

  bracketOnError mkSocket close $ \sock -> do
    setSocketOption sock ReuseAddr 1

    sock `bind` addrAddress ai
    sock `listen` maxListenQueue

    boundPort <- socketPort sock

    acceptThread <- forkIO $ acceptConnections sock handler

    return $ listener {
        listenerPoint  = TCP host boundPort
      , listenerCancel = killThread acceptThread >> close sock
      }

acceptConnections :: Socket -> ConnectionHandler -> IO ()
acceptConnections sock handler =
  forever $ mask $ \restore -> do
    (conn, addr) <- accept sock
    forkIO $ restore (serveConnection conn addr handler) `finally`
      (try $ close conn :: IO (Either SomeException ()))

serveConnection :: Socket -> SockAddr -> ConnectionHandler -> IO ()
serveConnection sock peerAddr connectionHandler = do
  setSocketOption sock KeepAlive 1

  peerName <- addrName peerAddr
  localPort <- socketPort sock

  let connectionName :: STM String
      connectionName = do
        peerHost <- hostName peerName
        return $ "port " ++ show localPort ++ " from " ++
          peerHost ++ ", port " ++ addrPort peerName

      input  = fromSocket sock maxBufferSize
      output = toSocket   sock

  connectionHandler connectionName (input, output)

data AddrName = AddrName {
    addrHostName :: TMVar (Maybe HostName)
  , addrNumeric  :: HostName
  , addrPort     :: ServiceName
  }

addrName :: SockAddr -> IO AddrName
addrName addr = do
  nameVar <- newEmptyTMVarIO

  forkIO $ do
    maybeHost <- try (fst `liftM` getNameInfo [NI_NAMEREQD] True False addr) >>=
                 either (\except -> let _ = except :: SomeException
                                    in return Nothing) return
    atomically $ putTMVar nameVar maybeHost

  (Just numericHost, Just port) <-
    getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True addr

  return $ AddrName nameVar numericHost port

hostName :: AddrName -> STM HostName
hostName addr =
  return . fromMaybe (addrNumeric addr) =<< readTMVar (addrHostName addr)
