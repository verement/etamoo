-- -*- Haskell -*-

module MOO.Database ( Database
                    , systemObject
                    , serverOptions
                    , maxStackDepth
                    , getServerOption'
                    , dbObject
                    , modifyObject
                    ) where

import Control.Concurrent.STM (STM)

import {-# SOURCE #-} MOO.Object (Object)
import {-# SOURCE #-} MOO.Task (MOO)
import MOO.Types

data Database
data ServerOptions

systemObject :: ObjId

serverOptions :: Database -> ServerOptions
maxStackDepth :: ServerOptions -> IntT

getServerOption' :: ObjId -> Id -> MOO (Maybe Value)

dbObject :: ObjId -> Database -> STM (Maybe Object)
modifyObject :: ObjId -> Database -> (Object -> STM Object) -> STM ()
