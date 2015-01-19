-- -*- Haskell -*-

module MOO.Connection (
    Connection
  , firstConnectionId
  , notify
  ) where

import {-# SOURCE #-} MOO.Task (MOO)
import MOO.Types (ObjId, StrT)

data Connection

firstConnectionId :: ObjId
notify :: ObjId -> StrT -> MOO Bool
