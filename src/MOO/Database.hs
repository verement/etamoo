
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module MOO.Database (
    Database
  , ServerOptions(..)
  , serverOptions
  , initDatabase
  , dbObjectRef
  , dbObject
  , maxObject
  , resetMaxObject
  , renumber
  , setObjects
  , addObject
  , deleteObject
  , modifyObject
  , allPlayers
  , setPlayer
  , getServerOption
  , getServerOption'
  , loadServerOptions
  , getServerMessage
  , writeMagic
  , checkMagic
  , loadDatabase
  , saveDatabase
  ) where

import Control.Applicative ((<$>))
import Control.Monad (forM, forM_, when, unless, (>=>))
import Data.ByteString (ByteString)
import Data.IntSet (IntSet)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Database.VCache (VCache, VSpace, VTx,
                        VCacheable(put, get), PVar, loadRootPVar,
                        newPVarsIO, newPVar, readPVarIO, readPVar, writePVar)

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.IntSet as IS
import qualified Data.Vector as V

import {-# SOURCE #-} MOO.Builtins (builtinFunctions)
import MOO.Object
import MOO.Task
import MOO.Types
import MOO.Util

import qualified MOO.List as Lst
import qualified MOO.String as Str

data Database = Database {
    objects       :: Vector (PVar (Maybe Object))
  , players       :: IntSet
  , serverOptions :: ServerOptions
} deriving Typeable

instance VCacheable Database where
  put db = do
    put $ VVector (objects db)
    put $ VIntSet (players db)

  get = do
    objects <- unVVector <$> get
    players <- unVIntSet <$> get
    return initDatabase { objects = objects, players = players }

initDatabase = Database {
    objects       = V.empty
  , players       = IS.empty
  , serverOptions = undefined
}

dbObjectRef :: ObjId -> Database -> Maybe (PVar (Maybe Object))
dbObjectRef oid = (V.!? oid) . objects

dbObject :: ObjId -> Database -> VTx (Maybe Object)
dbObject oid = maybe (return Nothing) readPVar . dbObjectRef oid

maxObject :: Database -> ObjId
maxObject = pred . V.length . objects

resetMaxObject :: Database -> VTx Database
resetMaxObject db = do
  newMaxObject <- findLastValid (maxObject db)
  return db { objects = V.take (succ newMaxObject) $ objects db }

  where findLastValid :: ObjId -> VTx ObjId
        findLastValid oid
          | oid >= 0  = dbObject oid db >>=
                        maybe (findLastValid $ pred oid) (return . const oid)
          | otherwise = return nothing

-- | Renumber an object in the database to be the least nonnegative object
-- number not currently in use pursuant to the renumber() built-in function.
renumber :: ObjId -> Database -> VTx (ObjId, Database)
renumber old db = do
  maybeNew <- findLeastUnused 0
  case maybeNew of
    Nothing  -> return (old, db)
    Just new -> do
      -- renumber database slot references
      let Just oldRef = dbObjectRef old db
          Just newRef = dbObjectRef new db
      Just obj <- readPVar oldRef
      writePVar oldRef Nothing
      writePVar newRef (Just obj)

      -- ask the object to fix up parent/children and location/contents
      renumberObject obj old new db

      -- fix up ownerships throughout entire database
      forM_ [0..maxObject db] $ \oid -> case dbObjectRef oid db of
        Just ref -> do
          maybeObj <- readPVar ref
          case maybeObj of
            Just obj -> do
              maybeNew <- renumberOwnership old new obj
              when (isJust maybeNew) $ writePVar ref maybeNew
            Nothing  -> return ()
        Nothing  -> return ()

      -- renumber player references (if any)
      let db' = setPlayer (objectIsPlayer obj) new $ setPlayer False old db

      return (new, db')

  where findLeastUnused :: ObjId -> VTx (Maybe ObjId)
        findLeastUnused new
          | new < old = dbObject new db >>= maybe (return $ Just new)
                        (const $ findLeastUnused $ succ new)
          | otherwise = return Nothing

setObjects :: VSpace -> [Maybe Object] -> Database -> IO Database
setObjects vspace objs db = do
  refs <- newPVarsIO vspace objs
  return db { objects = V.fromList refs }

addObject :: Object -> Database -> VTx Database
addObject obj db = do
  ref <- newPVar (Just obj)
  return db { objects = V.snoc (objects db) ref }

deleteObject :: ObjId -> Database -> VTx ()
deleteObject oid db =
  case dbObjectRef oid db of
    Just ref -> writePVar ref Nothing
    Nothing  -> return ()

modifyObject :: ObjId -> Database -> (Object -> VTx Object) -> VTx ()
modifyObject oid db f =
  case dbObjectRef oid db of
    Just ref -> readPVar ref >>= maybe (return ()) (f >=> writePVar ref . Just)
    Nothing  -> return ()

{-
isPlayer :: ObjId -> Database -> Bool
isPlayer oid db = oid `IS.member` players db
-}

allPlayers :: Database -> [ObjId]
allPlayers = IS.toList . players

setPlayer :: Bool -> ObjId -> Database -> Database
setPlayer isPlayer oid db = db { players = change oid (players db) }
  where change | isPlayer  = IS.insert
               | otherwise = IS.delete

data ServerOptions = Options {
    bgSeconds :: Int
    -- ^ The number of seconds allotted to background tasks

  , bgTicks :: Int
    -- ^ The number of ticks allotted to background tasks

  , connectTimeout :: IntT
    -- ^ The maximum number of seconds to allow an un-logged-in in-bound
    -- connection to remain open

  , defaultFlushCommand :: Text
    -- ^ The initial setting of each new connection’s flush command

  , fgSeconds :: Int
    -- ^ The number of seconds allotted to foreground tasks

  , fgTicks :: Int
    -- ^ The number of ticks allotted to foreground tasks

  , maxStackDepth :: Int
    -- ^ The maximum number of levels of nested verb calls

  , queuedTaskLimit :: Maybe Int
    -- ^ The default maximum number of tasks a player can have

  , nameLookupTimeout :: IntT
    -- ^ The maximum number of seconds to wait for a network hostname/address
    -- lookup

  , outboundConnectTimeout :: IntT
    -- ^ The maximum number of seconds to wait for an outbound network
    -- connection to successfully open

  , protectProperty :: Id -> Bool
    -- ^ Restrict reading of built-in property to wizards

  , protectFunction :: Id -> Bool
    -- ^ Restrict use of built-in function to wizards

  , supportNumericVerbnameStrings :: Bool
    -- ^ Enables use of an obsolete verb-naming mechanism
}

getServerOption :: Id -> MOO (Maybe Value)
getServerOption = getServerOption' systemObject

getServerOption' :: ObjId -> Id -> MOO (Maybe Value)
getServerOption' oid option = getServerOptions oid >>= ($ option)

getServerOptions :: ObjId -> MOO (Id -> MOO (Maybe Value))
getServerOptions oid = do
  serverOptions <- readProperty oid "server_options"
  return $ case serverOptions of
    Just (Obj oid) -> readProperty oid . fromId
    _              -> const (return Nothing)

getProtected :: (Id -> MOO (Maybe Value)) -> [Id] -> MOO (Id -> Bool)
getProtected getOption ids = do
  maybes <- forM ids $ fmap (fmap truthOf) . getOption . ("protect_" <>)
  let protectedSet = HS.fromList [ id | (id, Just True) <- zip ids maybes ]
  return (`HS.member` protectedSet)

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
  protectFunction <- getProtected option (HM.keys builtinFunctions)

  let options = Options {
          bgSeconds = case bgSeconds of
             Just (Int secs) | secs >= 1 -> fromIntegral secs
             _                           -> defaultBgSeconds
        , bgTicks = case bgTicks of
             Just (Int ticks) | ticks >= 100 -> fromIntegral ticks
             _                               -> defaultBgTicks
        , fgSeconds = case fgSeconds of
             Just (Int secs) | secs >= 1 -> fromIntegral secs
             _                           -> defaultFgSeconds
        , fgTicks = case fgTicks of
             Just (Int ticks) | ticks >= 100 -> fromIntegral ticks
             _                               -> defaultFgTicks

        , maxStackDepth = case maxStackDepth of
             Just (Int depth) | depth > 50 -> fromIntegral depth
             _                             -> defaultMaxStackDepth
        , queuedTaskLimit = case queuedTaskLimit of
             Just (Int limit) | limit >= 0 -> Just (fromIntegral limit)
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
             Just (Str cmd) -> Str.toText cmd
             Just _         -> ""
             Nothing        -> ".flush"

        , supportNumericVerbnameStrings =
             maybe False truthOf supportNumericVerbnameStrings

        , protectProperty = protectProperty
        , protectFunction = protectFunction
        }

  db <- getDatabase
  putDatabase db { serverOptions = options }

getServerMessage :: ObjId -> Id -> MOO [Text] -> MOO [Text]
getServerMessage oid msg def = do
  maybeValue <- getServerOption' oid msg
  case maybeValue of
    Just (Str s) -> return [Str.toText s]
    Just (Lst v) -> maybe (return []) return $ strings (Lst.toList v)
    Just _       -> return []
    Nothing      -> def
  where strings :: [Value] -> Maybe [Text]
        strings (v:vs) = case v of
          Str s -> (Str.toText s :) <$> strings vs
          _     -> Nothing
        strings [] = Just []

dbMagic :: ByteString
dbMagic = "EtaMOO database"

magicPVar :: VCache -> PVar ByteString
magicPVar vcache = loadRootPVar vcache "magic" ""

writeMagic :: VCache -> VTx ()
writeMagic vcache = writePVar (magicPVar vcache) dbMagic

checkMagic :: VCache -> IO ()
checkMagic vcache = do
  magic <- readPVarIO (magicPVar vcache)
  unless (magic == dbMagic) $ error "invalid database"

databasePVar :: VCache -> PVar Database
databasePVar vcache = loadRootPVar vcache "database" initDatabase

loadDatabase :: VCache -> IO Database
loadDatabase vcache = readPVarIO (databasePVar vcache)

saveDatabase :: VCache -> Database -> VTx ()
saveDatabase vcache = writePVar (databasePVar vcache)
