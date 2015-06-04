-- -*- Haskell -*-

module MOO.Database ( Database
                    , ServerOptions
                    , serverOptions
                    , getServerOption'
                    , dbObject
                    , modifyObject
                    , loadServerOptions
                    , fgSeconds
                    , bgSeconds
                    , fgTicks
                    , bgTicks
                    , maxStackDepth
                    , queuedTaskLimit
                    ) where

import Control.Concurrent.STM (STM)

import {-# SOURCE #-} MOO.Object (Object)
import {-# SOURCE #-} MOO.Task (MOO)
import MOO.Types

data Database
data ServerOptions

serverOptions :: Database -> ServerOptions
getServerOption' :: ObjId -> Id -> MOO (Maybe Value)

dbObject :: ObjId -> Database -> STM (Maybe Object)
modifyObject :: ObjId -> Database -> (Object -> STM Object) -> STM ()

loadServerOptions :: MOO ()

fgSeconds :: ServerOptions -> Int
bgSeconds :: ServerOptions -> Int

fgTicks :: ServerOptions -> Int
bgTicks :: ServerOptions -> Int

maxStackDepth :: ServerOptions -> Int
queuedTaskLimit :: ServerOptions -> Maybe Int
