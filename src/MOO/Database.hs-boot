-- -*- Haskell -*-

module MOO.Database ( Database
                    , ServerOptions
                    , Persistence
                    , serverOptions
                    , getServerOption'
                    , initDatabase
                    , dbObject
                    , modifyObject
                    , loadServerOptions
                    , fgSeconds
                    , bgSeconds
                    , fgTicks
                    , bgTicks
                    , maxStackDepth
                    , queuedTaskLimit
                    , protectProperty
                    , supportNumericVerbnameStrings
                    , persistenceVSpace
                    , persistenceDatabase
                    , persistenceConnected
                    ) where

import Database.VCache (VSpace, PVar, VTx)

import {-# SOURCE #-} MOO.Object (Object)
import {-# SOURCE #-} MOO.Task (MOO)
import MOO.Types

data Database
data ServerOptions
data Persistence

type Connected = [(ObjId, ObjId)]

serverOptions :: Database -> ServerOptions
getServerOption' :: ObjId -> Id -> MOO (Maybe Value)

initDatabase :: Database

dbObject :: ObjId -> Database -> VTx (Maybe Object)
modifyObject :: ObjId -> Database -> (Object -> VTx Object) -> VTx ()

loadServerOptions :: MOO ()

fgSeconds :: ServerOptions -> Int
bgSeconds :: ServerOptions -> Int

fgTicks :: ServerOptions -> Int
bgTicks :: ServerOptions -> Int

maxStackDepth :: ServerOptions -> Int
queuedTaskLimit :: ServerOptions -> Maybe Int

protectProperty :: ServerOptions -> Id -> Bool

supportNumericVerbnameStrings :: ServerOptions -> Bool

persistenceVSpace :: Persistence -> VSpace
persistenceDatabase :: Persistence -> PVar Database
persistenceConnected :: Persistence -> PVar Connected
