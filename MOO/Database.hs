
module MOO.Database ( Database
                    , initDatabase
                    , dbObjectRef
                    , dbObject
                    , maxObject
                    , setObjects
                    , modifyObject
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

dbObjectRef :: ObjId -> Database -> Maybe (TVar (Maybe Object))
dbObjectRef oid db
  | oid < 0 || oid >= V.length objs = Nothing
  | otherwise                       = Just (objs V.! oid)
  where objs = objects db

dbObject :: ObjId -> Database -> STM (Maybe Object)
dbObject oid db = maybe (return Nothing) readTVar $ dbObjectRef oid db

maxObject :: Database -> ObjId
maxObject db = V.length (objects db) - 1

setObjects :: [Maybe Object] -> Database -> IO Database
setObjects objs db = do
  tvarObjs <- mapM newTVarIO objs
  return db { objects = V.fromList tvarObjs }

modifyObject :: ObjId -> Database -> (Object -> Object) -> STM ()
modifyObject oid db f = do
  case dbObjectRef oid db of
    Nothing      -> return ()
    Just objTVar -> do
      maybeObject <- readTVar objTVar
      case maybeObject of
        Nothing  -> return ()
        Just obj -> writeTVar objTVar (Just $ f obj)

isPlayer :: ObjId -> Database -> Bool
isPlayer oid db = oid `IS.member` players db

allPlayers :: Database -> [ObjId]
allPlayers = IS.toList . players

setPlayer :: Bool -> ObjId -> Database -> Database
setPlayer yesno oid db = db { players = change (players db) }
  where change = (if yesno then IS.insert else IS.delete) oid
