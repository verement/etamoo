-- -*- Haskell -*-

module MOO.Connection (
    Connection
  , connectionObject
  , firstConnectionId
  , sendToConnection
  , notify
  , bootPlayer
  ) where

import Control.Concurrent.STM (STM)
import Data.Text (Text)

import {-# SOURCE #-} MOO.Task (MOO)
import MOO.Types (ObjId, StrT)

data Connection

connectionObject :: Connection -> ObjId
firstConnectionId :: ObjId
sendToConnection :: Connection -> Text -> STM ()
notify :: ObjId -> StrT -> MOO Bool
bootPlayer :: ObjId -> MOO ()
