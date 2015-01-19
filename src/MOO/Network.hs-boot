-- -*- Haskell -*-

module MOO.Network (
    Point(..)
  , Listener(..)
  ) where

import Network.Socket (PortNumber)

import MOO.Types (ObjId)

newtype Point = TCP PortNumber

data Listener = Listener {
    listenerObject        :: ObjId
  , listenerPoint         :: Point
  , listenerPrintMessages :: Bool

  , listenerCancel        :: IO ()
  }
