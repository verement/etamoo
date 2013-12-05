
module MOO.Database ( Database
                    , initDatabase
                    , dbObjectRef
                    , dbObject
                    , maxObject
                    , setObjects
                    , modifyObject
                    , allPlayers
                    , setPlayer
                    ) where

import Control.Concurrent.STM
import Data.Vector (Vector)
import Data.IntSet (IntSet)
import Data.Maybe (isJust)
import Data.Map (Map)

import qualified Data.Vector as V
import qualified Data.IntSet as IS

import MOO.Types
import MOO.Object
import MOO.Task

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

{-
isPlayer :: ObjId -> Database -> Bool
isPlayer oid db = oid `IS.member` players db
-}

allPlayers :: Database -> [ObjId]
allPlayers = IS.toList . players

setPlayer :: Bool -> ObjId -> Database -> Database
setPlayer yesno oid db = db { players = change (players db) }
  where change = (if yesno then IS.insert else IS.delete) oid

data ServerOptions = Options {
    bgSeconds :: IntT
    -- The number of seconds allotted to background tasks.

  , bgTicks :: IntT
    -- The number of ticks allotted to background tasks.

  , connectTimeout :: IntT
    -- The maximum number of seconds to allow an un-logged-in in-bound
    -- connection to remain open.

  , defaultFlushCommand :: StrT
    -- The initial setting of each new connection’s flush command.

  , fgSeconds :: IntT
    -- The number of seconds allotted to foreground tasks.

  , fgTicks :: IntT
    -- The number of ticks allotted to foreground tasks.

  , maxStackDepth :: IntT
    -- The maximum number of levels of nested verb calls.

  , queuedTaskLimit :: IntT
    -- The default maximum number of tasks a player can have.

  , nameLookupTimeout :: IntT
    -- The maximum number of seconds to wait for a network hostname/address
    -- lookup.

  , outboundConnectTimeout :: IntT
    -- The maximum number of seconds to wait for an outbound network
    -- connection to successfully open.

  , protectProperty :: Map StrT Bool
    -- Restrict reading of built-in property to wizards.

  , protectFunction :: Map StrT Bool
    -- Restrict use of built-in function to wizards.

  , supportNumericVerbnameStrings :: Bool
    -- Enables use of an obsolete verb-naming mechanism.
}
