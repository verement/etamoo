
{-# LANGUAGE OverloadedStrings #-}

module MOO.Database ( Database
                    , ServerOptions(..)
                    , serverOptions
                    , initDatabase
                    , systemObject
                    , dbObjectRef
                    , dbObject
                    , maxObject
                    , resetMaxObject
                    , setObjects
                    , addObject
                    , modifyObject
                    , allPlayers
                    , setPlayer
                    , getServerOption
                    , getServerOption'
                    , loadServerOptions
                    ) where

import Control.Concurrent.STM
import Control.Monad (forM, liftM)
import Data.Monoid ((<>))
import Data.Vector (Vector)
import Data.IntSet (IntSet)
import Data.Set (Set)

import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import MOO.Types
import MOO.Object
import MOO.Task
import {-# SOURCE #-} MOO.Builtins (builtinFunctions)

data Database = Database {
    objects       :: Vector (TVar (Maybe Object))
  , players       :: IntSet
  , serverOptions :: ServerOptions
  , queuedTasks   :: [Task]
}

initDatabase = Database {
    objects       = V.empty
  , players       = IS.empty
  , serverOptions = undefined
  , queuedTasks   = []
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

resetMaxObject :: Database -> STM Database
resetMaxObject db = do
  newMaxObject <- findLastValid (maxObject db)
  return db { objects = V.take (newMaxObject + 1) $ objects db }
  where findLastValid oid
          | oid >= 0  = dbObject oid db >>=
                        maybe (findLastValid $ oid - 1) (return . const oid)
          | otherwise = return (-1)

setObjects :: [Maybe Object] -> Database -> IO Database
setObjects objs db = do
  tvarObjs <- mapM newTVarIO objs
  return db { objects = V.fromList tvarObjs }

addObject :: Object -> Database -> STM Database
addObject obj db = do
  objTVar <- newTVar (Just obj)
  return db { objects = V.snoc (objects db) objTVar }

modifyObject :: ObjId -> Database -> (Object -> STM Object) -> STM ()
modifyObject oid db f =
  case dbObjectRef oid db of
    Nothing      -> return ()
    Just objTVar -> do
      maybeObject <- readTVar objTVar
      case maybeObject of
        Nothing  -> return ()
        Just obj -> writeTVar objTVar . Just =<< f obj

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

  , queuedTaskLimit :: Maybe IntT
    -- The default maximum number of tasks a player can have.

  , nameLookupTimeout :: IntT
    -- The maximum number of seconds to wait for a network hostname/address
    -- lookup.

  , outboundConnectTimeout :: IntT
    -- The maximum number of seconds to wait for an outbound network
    -- connection to successfully open.

  , protectProperty :: Set Id
    -- Restrict reading of built-in property to wizards.

  , protectFunction :: Set Id
    -- Restrict use of built-in function to wizards.

  , supportNumericVerbnameStrings :: Bool
    -- Enables use of an obsolete verb-naming mechanism.
}

systemObject :: ObjId
systemObject = 0

getServerOptions :: ObjId -> MOO (Id -> MOO (Maybe Value))
getServerOptions oid = do
  serverOptions <- readProperty oid "server_options"
  return $ case serverOptions of
    Just (Obj oid) -> readProperty oid
    _              -> const (return Nothing)

getServerOption :: Id -> MOO (Maybe Value)
getServerOption = getServerOption' systemObject

getServerOption' :: ObjId -> Id -> MOO (Maybe Value)
getServerOption' oid option = getServerOptions oid >>= ($ option)

getProtected :: (Id -> MOO (Maybe Value)) -> [Id] -> MOO (Set Id)
getProtected getOption ids = do
  maybes <- forM ids $ liftM (fmap truthOf) . getOption . ("protect_" <>)
  return $ S.fromList [ id | (id, Just True) <- zip ids maybes ]

loadServerOptions :: MOO ()
loadServerOptions = do
  option <- getServerOptions systemObject

  bgSeconds <- option "bg_seconds"
  bgTicks   <- option "bg_ticks"
  fgSeconds <- option "fg_seconds"
  fgTicks   <- option "fg_ticks"

  maxStackDepth   <- option "max_stack_depth"
  queuedTaskLimit <- option "queued_task_limit"

  connectTimeout         <- option "connect_timeout"
  outboundConnectTimeout <- option "outbound_connect_timeout"
  nameLookupTimeout      <- option "name_lookup_timeout"

  defaultFlushCommand <- option "default_flush_command"

  supportNumericVerbnameStrings <- option "support_numeric_verbname_strings"

  protectProperty <- getProtected option builtinProperties
  protectFunction <- getProtected option (M.keys builtinFunctions)

  let options = Options {
          bgSeconds = case bgSeconds of
             Just (Int secs) | secs >= 1 -> secs
             _                           -> 3
        , bgTicks = case bgTicks of
             Just (Int ticks) | ticks >= 100 -> ticks
             _                               -> 15000
        , fgSeconds = case fgSeconds of
             Just (Int secs) | secs >= 1 -> secs
             _                           -> 5
        , fgTicks = case fgTicks of
             Just (Int ticks) | ticks >= 100 -> ticks
             _                               -> 30000

        , maxStackDepth = case maxStackDepth of
             Just (Int depth) | depth > 50 -> depth
             _                             -> 50
        , queuedTaskLimit = case queuedTaskLimit of
             Just (Int limit) | limit >= 0 -> Just limit
             _                             -> Nothing

        , connectTimeout = case connectTimeout of
             Just (Int secs) | secs > 0 -> secs
             _                          -> 300
        , outboundConnectTimeout = case outboundConnectTimeout of
             Just (Int secs) | secs > 0 -> secs
             _                          -> 5
        , nameLookupTimeout = case nameLookupTimeout of
             Just (Int secs) | secs >= 0 -> secs
             _                           -> 5

        , defaultFlushCommand = case defaultFlushCommand of
             Just (Str cmd) -> cmd
             _              -> ".flush"

        , supportNumericVerbnameStrings = case supportNumericVerbnameStrings of
             Just v -> truthOf v
             _      -> False

        , protectProperty = protectProperty
        , protectFunction = protectFunction
        }

  db <- getDatabase
  putDatabase db { serverOptions = options }
