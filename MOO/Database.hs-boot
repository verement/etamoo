-- -*- Haskell -*-

module MOO.Database ( Database
                    , dbObject
                    ) where

import Control.Concurrent.STM (STM)
import MOO.Types (ObjId)
import MOO.Object (Object)

data Database

dbObject :: ObjId -> Database -> STM (Maybe Object)
