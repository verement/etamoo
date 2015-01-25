
{-# LANGUAGE OverloadedStrings #-}

module MOO.Server ( startServer ) where

import Control.Concurrent (takeMVar)
import Control.Concurrent.STM (TVar, atomically, readTVarIO, readTVar)
import Control.Monad (forM_)
import Data.Text (Text)
import Network (withSocketsDo)
import System.Posix (installHandler, sigPIPE, Handler(..))

import qualified Data.Map as M
import qualified Data.Text as T

import MOO.Builtins
import MOO.Connection
import MOO.Database.LambdaMOO
import MOO.Task
import MOO.Types
import MOO.Network
import MOO.Object

-- | Start the main server and create the first listening point.
startServer :: Maybe FilePath -> FilePath -> FilePath ->
               Bool -> (TVar World -> Point) -> IO ()
startServer logFile inputDB outputDB outboundNet pf = withSocketsDo $ do
  either error return verifyBuiltins
  installHandler sigPIPE Ignore Nothing

  db <- loadLMDatabase inputDB >>= either (error . show) return

  world' <- newWorld db outboundNet

  runTask =<< newTask world' nothing
    (resetLimits True >> callSystemVerb "server_started" [] >> return zero)

  createListener world' systemObject (pf world') True
  putStrLn "Listening for connections..."

  world <- readTVarIO world'
  takeMVar (shutdownMessage world) >>= shutdownServer world'

shutdownServer :: TVar World -> Text -> IO ()
shutdownServer world' message = do
  atomically $ do
    world <- readTVar world'

    forM_ (M.elems $ connections world) $ \conn -> do
      sendToConnection conn $ T.concat ["*** Shutting down: ", message, " ***"]
      closeConnection conn

  -- Give connections time to close
  delay 5000000

  return ()
