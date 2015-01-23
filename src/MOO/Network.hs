
{-# LANGUAGE OverloadedStrings #-}

module MOO.Network (
    Point(..)
  , Listener(..)
  , HostName
  , PortNumber
  , value2point
  , point2value
  , createListener
  , listen
  , unlisten
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar)
import Control.Exception (try)
import Control.Monad (when)
import System.IO.Error (isPermissionError)

import MOO.Connection (connectionHandler)
import MOO.Network.Console (createConsoleListener)
import MOO.Network.TCP (HostName, PortNumber, createTCPListener)
import MOO.Object
import {-# SOURCE #-} MOO.Task
import MOO.Types

import qualified Data.Map as M

data Point = Console (TVar World) | TCP (Maybe HostName) PortNumber
           deriving (Eq)

instance Ord Point where
  TCP _ port1 `compare` TCP _ port2 = port1 `compare` port2
  TCP{}       `compare` _           = GT
  Console{}   `compare` Console{}   = EQ
  Console{}   `compare` _           = LT

data Listener = Listener {
    listenerObject        :: ObjId
  , listenerPoint         :: Point
  , listenerPrintMessages :: Bool

  , listenerCancel        :: IO ()
  }

initListener = Listener {
    listenerObject        = systemObject
  , listenerPoint         = TCP Nothing 0
  , listenerPrintMessages = True

  , listenerCancel        = return ()
  }

value2point :: Value -> MOO Point
value2point value = do
  world <- getWorld
  case value of
    Int port      -> return $ TCP (bindAddress world) (fromIntegral port)
    Str "Console" -> Console `fmap` getWorld'
    _             -> raise E_TYPE

point2value :: Point -> Value
point2value point = case point of
  TCP _ port  -> Int (fromIntegral port)
  Console{}   -> Str "Console"

createListener :: TVar World -> ObjId -> Point -> Bool -> IO Listener
createListener world' object point printMessages = do
  let listener = initListener {
          listenerObject        = object
        , listenerPoint         = point
        , listenerPrintMessages = printMessages
        }
      handler = connectionHandler world' object printMessages

  listener <- case point of
    TCP{}     -> createTCPListener     listener handler
    Console{} -> createConsoleListener listener handler

  let canon = listenerPoint listener
  atomically $ modifyTVar world' $ \world -> world {
    listeners = M.insert canon listener (listeners world) }

  return listener

listen :: ObjId -> Point -> Bool -> MOO Point
listen object point printMessages = do
  world <- getWorld
  when (point `M.member` listeners world) $ raise E_INVARG

  world' <- getWorld'
  result <- requestIO $ try $ createListener world' object point printMessages
  case result of
    Left err | isPermissionError err -> raise E_PERM
             | otherwise             -> raise E_QUOTA
    Right listener                   -> return (listenerPoint listener)

unlisten :: Point -> MOO ()
unlisten point = do
  world <- getWorld
  case point `M.lookup` listeners world of
    Just Listener { listenerCancel = cancelListener } -> do
      putWorld world { listeners = M.delete point (listeners world) }
      delayIO cancelListener
    Nothing -> raise E_INVARG
