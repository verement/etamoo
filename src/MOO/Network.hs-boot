-- -*- Haskell -*-

module MOO.Network ( Listener
                   , Connection
                   , PortNumber
                   , notify
                   ) where

import Data.Text (Text)
import Network (PortNumber)

import MOO.Types (ObjId)
import {-# SOURCE #-} MOO.Task (MOO)

data Listener
data Connection

notify :: ObjId -> Text -> MOO ()
