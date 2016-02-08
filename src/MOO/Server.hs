
{-# LANGUAGE OverloadedStrings #-}

module MOO.Server (startServer, importDatabase, exportDatabase) where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Concurrent (takeMVar, putMVar, getNumCapabilities, threadDelay)
import Control.Concurrent.Async (async, withAsync, waitEither, wait)
import Control.Concurrent.STM (STM, TVar, atomically, retry, newEmptyTMVarIO,
                               tryPutTMVar, putTMVar, tryTakeTMVar, takeTMVar,
                               readTVarIO, readTVar, modifyTVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, void, unless)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.IO (hPutStrLn)
import Data.Time (getCurrentTime, utcToLocalZonedTime, formatTime,
                  defaultTimeLocale)
import Database.VCache (openVCache, vcache_space, readPVarIO)
import Database.VCache.Cache (setVRefsCacheLimit)
import Network (withSocketsDo)
import Pipes (Pipe, runEffect, (>->), for, cat, lift, yield)
import Pipes.Concurrent (spawn', unbounded, send, fromInput)
import System.IO (IOMode(ReadMode, AppendMode), BufferMode(LineBuffering),
                  Handle, openFile, stderr, hSetBuffering, hClose)
import System.Posix (handleToFd, dupTo, stdInput, stdOutput, stdError,
                     sigPIPE, sigHUP, sigINT, sigTERM, sigUSR1, sigUSR2,
                     openFd, closeFd, OpenMode(ReadWrite), defaultFileFlags,
                     installHandler, Handler(Ignore, Catch, CatchOnce))

import qualified Data.Map as M
import qualified Data.Text as T

import MOO.Builtins
import MOO.Builtins.Match (verifyPCRE)
import MOO.Connection
import MOO.Database
import MOO.Database.LambdaMOO
import MOO.Network
import MOO.Object
import MOO.Task
import MOO.Types
import MOO.Util
import MOO.Version

-- | Maximum database size, in megabytes
maxVCacheSize :: Int
maxVCacheSize = 2000

-- | Start the main server and create the first listening point.
startServer :: Maybe FilePath -> FilePath -> Int ->
               Bool -> (TVar World -> Point) -> IO ()
startServer logFile dbFile cacheSize outboundNet pf = withSocketsDo $ do
  verifyPCRE
  either error return verifyBuiltins

  installHandler sigPIPE Ignore Nothing

  (stmLogger, stopLogger) <- startLogger logFile
  let writeLog = atomically . stmLogger

  openFile dbFile ReadMode >>= hClose  -- ensure file exists
  vcache <- openVCache maxVCacheSize dbFile

  setVRefsCacheLimit (vcache_space vcache) (cacheSize * 1000 * 1000)

  numCapabilities <- getNumCapabilities

  writeLog $ "CMDLINE: Outbound network connections " <>
    if outboundNet then "enabled." else "disabled."

  mapM_ writeLog [
      "STARTING: Server version " <> serverVersion
    , "          (Using TCP/IP with IPv6 support)"
    , "          (Task timeouts not measured)"
    , "          (Multithreading over " <>
      pluralize numCapabilities "processor core" <> ")"
    , "          (Using " <> T.pack (show cacheSize) <> " MB database cache)"
    ]

  p <- loadPersistence vcache

  checkpoint <- ctime . unVUTCTime =<< readPVarIO (persistenceCheckpoint p)
  writeLog $ "LOADING: Database checkpoint from " <> T.pack checkpoint

  world' <- newWorld stmLogger p outboundNet

  redirectStdFd

  (checkpoint, stopCheckpointer) <- startCheckpointer world'
  atomically $ modifyTVar world' $ \world -> world { checkpoint = checkpoint }

  let shutdownHandler = CatchOnce $ do
        var <- shutdownMessage <$> readTVarIO world'
        putMVar var "shutdown signal received"
      checkpointHandler = Catch $ atomically checkpoint
    in do installHandler sigHUP  Ignore            Nothing
          installHandler sigINT  shutdownHandler   Nothing
          installHandler sigTERM shutdownHandler   Nothing
          installHandler sigUSR1 shutdownHandler   Nothing  -- remote shutdown
          installHandler sigUSR2 checkpointHandler Nothing  -- remote checkpoint

  connected <- readPVarIO (persistenceConnected p)
  unless (null connected) $ do
    writeLog $ "Disconnecting formerly active connections: " <>
      toLiteral (objectList $ map fst connected)
    forM_ connected $ \(player, listener) ->
      doDisconnected world' listener player Disconnected

  runTask =<< newTask world' nothing
    (resetLimits True >> callSystemVerb "server_started" [] >> return zero)

  createListener world' systemObject (pf world') True

  -- Normal server operation: wait for a shutdown signal
  message <- takeMVar . shutdownMessage =<< readTVarIO world'

  stopCheckpointer
  shutdownServer world' message

  writeLog "Synchronizing database..."
  syncPersistence p
  writeLog "Finished"

  stopLogger

  where pluralize :: Int -> Text -> Text
        pluralize n what = T.concat $
                           [ T.pack (show n), " ", what ] ++ [ "s" | n /= 1 ]

shutdownServer :: TVar World -> Text -> IO ()
shutdownServer world' message = do
  atomically $ do
    world <- readTVar world'

    writeLog world $ "SHUTDOWN: " <> message

    forM_ (M.elems $ connections world) $ \conn -> do
      -- XXX probably not good to send to ALL connections
      sendToConnection conn $ "*** Shutting down: " <> message <> " ***"
      closeConnection conn

  -- Wait for all connections to close
  atomically $ do
    world <- readTVar world'
    unless (M.null $ connections world) retry

  return ()

redirectStdFd :: IO ()
redirectStdFd = do
  devNull <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
  devNull `dupTo` stdInput
  devNull `dupTo` stdOutput
  closeFd devNull

startLogger :: Maybe FilePath -> IO (Text -> STM (), IO ())
startLogger dest = do
  handle <- maybe (return stderr) openLogFile dest
  hSetBuffering handle LineBuffering

  (output, input, seal) <- spawn' unbounded

  logger <- async $ do
    runEffect $
      fromInput input >-> timestamp >-> for cat (lift . hPutStrLn handle)
    hClose handle

  let writeLog   = void . send output
      stopLogger = atomically seal >> wait logger

  return (writeLog, stopLogger)

  where openLogFile :: FilePath -> IO Handle
        openLogFile path = do
          fd <- handleToFd =<< openFile path AppendMode
          fd `dupTo` stdError >> closeFd fd
          return stderr

        timestamp :: Pipe Text Text IO ()
        timestamp = for cat $ \line -> do
          zonedTime <- lift $ utcToLocalZonedTime =<< getCurrentTime
          let timestamp = formatTime defaultTimeLocale "%b %_d %T: " zonedTime
          yield $ T.pack timestamp <> line

startCheckpointer :: TVar World -> IO (STM (), IO ())
startCheckpointer world' = do
  (writeLog, p) <- atomically $ (writeLog &&& persistence) <$> readTVar world'
  signal <- newEmptyTMVarIO

  let verbCall :: StrT -> [Value] -> IO ()
      verbCall name args =
        void $ runTask =<< newTask world' nothing
        (resetLimits False >> callSystemVerb name args >> return zero)

      checkpointerLoop :: IO ()
      checkpointerLoop = do
        dumpInterval <- runTask =<< newTask world' nothing
                        (fromMaybe zero <$>
                         readProperty systemObject "dump_interval")
        let interval = case dumpInterval >>= toMicroseconds of
              Just usecs | usecs >= 60 * 1000000 -> fromIntegral usecs
              _                                  -> 3600 * 1000000

        shouldExit <- withAsync (threadDelay interval) $ \delayAsync ->
          withAsync (atomically $ takeTMVar signal) $ \signalAsync ->
          waitEither delayAsync signalAsync
        case shouldExit of
          Right True -> return ()
          _          -> do
            atomically $ writeLog "CHECKPOINTING database..."
            verbCall "checkpoint_started" []
            success <- either (\e -> let _ = e :: SomeException in False)
                              (const True) <$> try (syncPersistence p)
            atomically $ writeLog "CHECKPOINTING finished"
            verbCall "checkpoint_finished" [truthValue success]

            checkpointerLoop

  checkpointer <- async checkpointerLoop

  let checkpoint = void $ tryPutTMVar signal False
      stopCheckpointer = do
        atomically $ tryTakeTMVar signal >> putTMVar signal True
        wait checkpointer

  return (checkpoint, stopCheckpointer)

-- | Create a native EtaMOO database from an existing LambdaMOO-format
-- database.
importDatabase :: FilePath -> FilePath -> IO ()
importDatabase lambdaDB etaDB = do
  vcache <- openVCache maxVCacheSize etaDB
  let vspace = vcache_space vcache

  let writeLog = putStrLn . T.unpack
  loadLMDatabase vspace lambdaDB writeLog >>=
    either (error . show) (saveDatabase vcache)

-- | Write a LambdaMOO-format database corresponding to a native EtaMOO
-- database.
exportDatabase :: FilePath -> FilePath -> IO ()
exportDatabase etaDB lambdaDB = do
  openFile etaDB ReadMode >>= hClose  -- ensure file exists

  p <- loadPersistence =<< openVCache maxVCacheSize etaDB

  db        <- readPVarIO (persistenceDatabase  p)
  connected <- readPVarIO (persistenceConnected p)

  saveLMDatabase (persistenceVSpace p) lambdaDB (db, connected)
