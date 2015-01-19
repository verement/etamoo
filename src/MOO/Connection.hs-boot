-- -*- Haskell -*-

module MOO.Connection (
    Connection
  , defaultConnectionEncoding
  , firstConnectionId
  , notify
  ) where

import System.IO (TextEncoding)

import {-# SOURCE #-} MOO.Task (MOO)
import MOO.Types (ObjId, StrT)

data Connection

defaultConnectionEncoding :: IO TextEncoding
firstConnectionId :: ObjId
notify :: ObjId -> StrT -> MOO Bool
