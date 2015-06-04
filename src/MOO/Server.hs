
{-# LANGUAGE OverloadedStrings #-}

module MOO.Server ( startServer ) where

import Control.Concurrent (takeMVar, forkIO)
import Control.Concurrent.STM (STM, TVar, atomically, readTVarIO, readTVar)
import Control.Monad (forM_, void)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.IO (hPutStrLn)
import Data.Time (getCurrentTime, utcToLocalZonedTime, formatTime,
                  defaultTimeLocale)
import Network (withSocketsDo)
import Pipes (Pipe, runEffect, (>->), for, cat, lift, yield)
import Pipes.Concurrent (spawn, unbounded, send, fromInput)
import System.IO (IOMode(AppendMode), BufferMode(LineBuffering),
                  openFile, stderr, hSetBuffering)
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

  stmLogger <- startLogger logFile
  let writeLog = atomically . stmLogger

  writeLog $ "CMDLINE: Outbound network connections " <>
    if outboundNet then "enabled." else "disabled."

  mapM_ writeLog [
      "STARTING: Version " <> serverVersion <> " of the LambdaMOO server"
    , "          (Using TCP/IP with IPv6 support)"
    , "          (Task timeouts not measured.)"
    ]

  db <- loadLMDatabase inputDB writeLog >>= either (error . show) return

  world' <- newWorld stmLogger db outboundNet

  runTask =<< newTask world' nothing
    (resetLimits True >> callSystemVerb "server_started" [] >> return zero)

  createListener world' systemObject (pf world') True

  world <- readTVarIO world'
  takeMVar (shutdownMessage world) >>= shutdownServer world'

shutdownServer :: TVar World -> Text -> IO ()
shutdownServer world' message = do
  atomically $ do
    world <- readTVar world'

    writeLog world "SHUTDOWN: shutdown signal received"

    forM_ (M.elems $ connections world) $ \conn -> do
      sendToConnection conn $ "*** Shutting down: " <> message <> " ***"
      closeConnection conn

  -- Give connections time to close
  delay 5000000

  return ()

startLogger :: Maybe FilePath -> IO (Text -> STM ())
startLogger dest = do
  handle <- case dest of
    Just path -> openFile path AppendMode
    Nothing   -> return stderr

  hSetBuffering handle LineBuffering

  (output, input) <- spawn unbounded

  forkIO $ runEffect $
    fromInput input >-> timestamp >-> for cat (lift . hPutStrLn handle)

  return (void . send output)

  where timestamp :: Pipe Text Text IO ()
        timestamp = for cat $ \line -> do
          zonedTime <- lift (getCurrentTime >>= utcToLocalZonedTime)
          let timestamp = formatTime defaultTimeLocale "%b %_d %T: " zonedTime
          yield $ T.pack timestamp <> line
