
module MOO.Database ( Database
                    , initDatabase
                    , dbObject
                    , maxObject
                    , setObjects
                    , isPlayer
                    , allPlayers
                    , setPlayer
                    ) where

import Control.Concurrent.STM
import Data.Vector (Vector)
import Data.IntSet (IntSet)
import Data.Maybe (isJust)

import qualified Data.Vector as V
import qualified Data.IntSet as IS

import MOO.Object
import MOO.Task
import MOO.Types

data Database = Database {
    objects     :: Vector (TVar (Maybe Object))
  , players     :: IntSet
  , queuedTasks :: [Task]
}

initDatabase = Database {
    objects     = V.empty
  , players     = IS.empty
  , queuedTasks = []
}

dbObject :: ObjId -> Database -> STM (Maybe Object)
dbObject oid db
  | oid < 0 || oid >= V.length objs = return Nothing
  | otherwise                       = readTVar (objs V.! oid)
  where objs = objects db

maxObject :: Database -> ObjId
maxObject db = V.length (objects db) - 1

setObjects :: [Maybe Object] -> Database -> IO Database
setObjects objs db = do
  tvarObjs <- mapM newTVarIO objs
  return db { objects = V.fromList tvarObjs }

isPlayer :: ObjId -> Database -> Bool
isPlayer oid db = oid `IS.member` players db

allPlayers :: Database -> [ObjId]
allPlayers = IS.toList . players

setPlayer :: Bool -> ObjId -> Database -> Database
setPlayer yesno oid db = db { players = change (players db) }
  where change = (if yesno then IS.insert else IS.delete) oid
