-- -*- Haskell -*-

module MOO.Network (
    Point(..)
  , Listener(..)
  , HostName
  , createListener
  , unlisten
  ) where

import Control.Concurrent.STM (TVar)
import Network.Socket (HostName, PortNumber)

import {-# SOURCE #-} MOO.Task
import MOO.Types (ObjId)

data Point = TCP (Maybe HostName) PortNumber

data Listener = Listener {
    listenerObject        :: ObjId
  , listenerPoint         :: Point
  , listenerPrintMessages :: Bool

  , listenerCancel        :: IO ()
  }

createListener :: TVar World -> ObjId -> Point -> Bool -> IO Listener
unlisten :: Point -> MOO ()
