
{-# LANGUAGE OverloadedStrings #-}

module MOO.Network (
    Point(..)
  , Listener(..)
  , value2point
  , point2value
  , listen
  , unlisten
  ) where

import Control.Concurrent.STM (TVar)
import Control.Exception (try)
import Control.Monad (when)
import System.IO.Error (isPermissionError)

import MOO.Connection (connectionHandler)
import MOO.Network.TCP (PortNumber, createTCPListener)
import MOO.Object
import {-# SOURCE #-} MOO.Task
import MOO.Types

import qualified Data.Map as M

newtype Point = TCP PortNumber
              deriving (Eq, Ord)

data Listener = Listener {
    listenerObject        :: ObjId
  , listenerPoint         :: Point
  , listenerPrintMessages :: Bool

  , listenerCancel        :: IO ()
  }

initListener = Listener {
    listenerObject        = systemObject
  , listenerPoint         = TCP 0
  , listenerPrintMessages = True

  , listenerCancel        = return ()
  }

value2point :: Value -> MOO Point
value2point value = case value of
  Int port      -> return $ TCP (fromIntegral port)
  _             -> raise E_TYPE

point2value :: Point -> Value
point2value point = case point of
  TCP port -> Int (fromIntegral port)

createListener :: TVar World -> ObjId -> Point -> Bool -> IO Listener
createListener world' object point printMessages = do
  let listener = initListener {
          listenerObject        = object
        , listenerPoint         = point
        , listenerPrintMessages = printMessages
        }
      handler = connectionHandler world' object printMessages

  case point of
    TCP{} -> createTCPListener listener handler

listen :: ObjId -> Point -> Bool -> MOO Point
listen object point printMessages = do
  world <- getWorld
  when (point `M.member` listeners world) $ raise E_INVARG

  world' <- getWorld'
  result <- requestIO $ try $ createListener world' object point printMessages
  case result of
    Left err | isPermissionError err -> raise E_PERM
             | otherwise             -> raise E_QUOTA
    Right listener -> do
      let canon = listenerPoint listener
      putWorld world { listeners = M.insert canon listener (listeners world) }
      return canon

unlisten :: Point -> MOO ()
unlisten point = do
  world <- getWorld
  case point `M.lookup` listeners world of
    Just Listener { listenerCancel = cancelListener } -> do
      putWorld world { listeners = M.delete point (listeners world) }
      requestIO cancelListener
    Nothing -> raise E_INVARG
