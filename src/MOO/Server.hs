
{-# LANGUAGE OverloadedStrings #-}

module MOO.Server ( startServer ) where

import Control.Concurrent (newEmptyMVar, takeMVar, putMVar,
                           forkIO, getNumCapabilities)
import Control.Concurrent.STM (STM, TVar, atomically, retry,
                               readTVarIO, readTVar)
import Control.Exception (finally)
import Control.Monad (forM_, void, unless)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.IO (hPutStrLn)
import Data.Time (getCurrentTime, utcToLocalZonedTime, formatTime,
                  defaultTimeLocale)
import Network (withSocketsDo)
import Pipes (Pipe, runEffect, (>->), for, cat, lift, yield)
import Pipes.Concurrent (spawn', unbounded, send, fromInput)
import System.IO (IOMode(AppendMode), BufferMode(LineBuffering),
                  openFile, stderr, hSetBuffering, hClose)
import System.Posix (installHandler, sigPIPE, Handler(Ignore))

import qualified Data.Map as M
import qualified Data.Text as T

import MOO.Builtins
import MOO.Connection
import MOO.Database.LambdaMOO
import MOO.Network
import MOO.Object
import MOO.Task
import MOO.Types
import MOO.Version

-- | Start the main server and create the first listening point.
startServer :: Maybe FilePath -> FilePath -> FilePath ->
               Bool -> (TVar World -> Point) -> IO ()
startServer logFile inputDB outputDB outboundNet pf = withSocketsDo $ do
  either error return verifyBuiltins
  installHandler sigPIPE Ignore Nothing

  (stmLogger, stopLogger) <- startLogger logFile
  let writeLog = atomically . stmLogger

  numCapabilities <- getNumCapabilities

  writeLog $ "CMDLINE: Outbound network connections " <>
    if outboundNet then "enabled." else "disabled."

  mapM_ writeLog [
      "STARTING: Version " <> serverVersion <> " of the LambdaMOO server"
    , "          (Using TCP/IP with IPv6 support)"
    , "          (Task timeouts not measured)"
    , "          (Multithreading over " <>
      pluralize numCapabilities "processor core" <> ")"
    ]

  db <- loadLMDatabase inputDB writeLog >>= either (error . show) return

  world' <- newWorld stmLogger db outboundNet

  runTask =<< newTask world' nothing
    (resetLimits True >> callSystemVerb "server_started" [] >> return zero)

  createListener world' systemObject (pf world') True

  world <- readTVarIO world'
  takeMVar (shutdownMessage world) >>= shutdownServer world'

  stopLogger

  where pluralize :: Int -> Text -> Text
        pluralize n what = T.concat $
                           [ T.pack (show n), " ", what ] ++ [ "s" | n /= 1 ]

shutdownServer :: TVar World -> Text -> IO ()
shutdownServer world' message = do
  atomically $ do
    world <- readTVar world'

    writeLog world "SHUTDOWN: shutdown signal received"

    forM_ (M.elems $ connections world) $ \conn -> do
      -- XXX probably not good to send to ALL connections
      sendToConnection conn $ "*** Shutting down: " <> message <> " ***"
      closeConnection conn

  -- Wait for all connections to close
  atomically $ do
    world <- readTVar world'
    unless (M.null $ connections world) retry

  return ()

startLogger :: Maybe FilePath -> IO (Text -> STM (), IO ())
startLogger dest = do
  handle <- maybe (return stderr) (`openFile` AppendMode) dest
  hSetBuffering handle LineBuffering

  (output, input, seal) <- spawn' unbounded
  done <- newEmptyMVar

  forkIO $ (`finally` putMVar done ()) $ do
    runEffect $
      fromInput input >-> timestamp >-> for cat (lift . hPutStrLn handle)
    hClose handle

  let writeLog   = void . send output
      stopLogger = atomically seal >> takeMVar done

  return (writeLog, stopLogger)

  where timestamp :: Pipe Text Text IO ()
        timestamp = for cat $ \line -> do
          zonedTime <- lift (getCurrentTime >>= utcToLocalZonedTime)
          let timestamp = formatTime defaultTimeLocale "%b %_d %T: " zonedTime
          yield $ T.pack timestamp <> line
