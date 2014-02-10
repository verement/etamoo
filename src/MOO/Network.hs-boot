-- -*- Haskell -*-

module MOO.Network ( Listener
                   , Connection
                   , PortNumber
                   , notify
                   ) where

import Network (PortNumber)

import {-# SOURCE #-} MOO.Task (MOO)
import MOO.Types (ObjId, StrT)

data Listener
data Connection

notify :: ObjId -> StrT -> MOO ()
