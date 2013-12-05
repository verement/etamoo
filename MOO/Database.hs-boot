-- -*- Haskell -*-

module MOO.Database ( Database
                    , dbObject
                    , modifyObject
                    , isPlayer
                    ) where

import Control.Concurrent.STM (STM)
import MOO.Types (ObjId)
import MOO.Object (Object)

data Database

dbObject :: ObjId -> Database -> STM (Maybe Object)
modifyObject :: ObjId -> Database -> (Object -> Object) -> STM ()
isPlayer :: ObjId -> Database -> Bool
