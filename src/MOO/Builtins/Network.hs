
{-# LANGUAGE OverloadedStrings #-}

module MOO.Builtins.Network ( builtins ) where

import Control.Monad (liftM)
import Control.Monad.State (gets)
import Data.Time (UTCTime, diffUTCTime)

import qualified Data.Map as M

import MOO.Builtins.Common
import MOO.Network
import MOO.Object (systemObject)
import MOO.Task
import MOO.Types

import qualified MOO.String as Str

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- | § 4.4.4 Operations on Network Connections
builtins :: [Builtin]
builtins = [
    bf_connected_players
  , bf_connected_seconds
  , bf_idle_seconds
  , bf_notify
  , bf_buffered_output_length
  , bf_read
  , bf_force_input
  , bf_flush_input
  , bf_output_delimiters
  , bf_boot_player
  , bf_connection_name
  , bf_set_connection_option
  , bf_connection_options
  , bf_connection_option
  , bf_open_network_connection
  , bf_listen
  , bf_unlisten
  , bf_listeners
  ]

bf_connected_players = Builtin "connected_players" 0 (Just 1)
                       [TAny] TLst $ \optional -> do
  let [include_all] = booleanDefaults optional [False]

  world <- getWorld
  let objects = M.keys $ connections world
  return $ objectList $ if include_all then objects else filter (>= 0) objects

connectionSeconds :: ObjId -> (Maybe Connection -> Maybe UTCTime) -> MOO Value
connectionSeconds oid f = do
  world <- getWorld
  case f $ M.lookup oid (connections world) of
    Just utcTime -> secondsSince utcTime
    Nothing      -> raise E_INVARG

  where secondsSince :: UTCTime -> MOO Value
        secondsSince utcTime = do
          now <- gets startTime
          return (Int $ floor $ now `diffUTCTime` utcTime)

bf_connected_seconds = Builtin "connected_seconds" 1 (Just 1)
                       [TObj] TInt $ \[Obj player] ->
  connectionSeconds player (connectionEstablishedTime =<<)

bf_idle_seconds = Builtin "idle_seconds" 1 (Just 1)
                  [TObj] TInt $ \[Obj player] ->
  connectionSeconds player (connectionActivityTime `fmap`)

bf_notify = Builtin "notify" 2 (Just 3)
            [TObj, TStr, TAny] TAny $ \(Obj conn : Str string : optional) -> do
  let [no_flush] = booleanDefaults optional [False]

  notify conn string
  return $ truthValue True

bf_buffered_output_length = Builtin "buffered_output_length" 0 (Just 1)
                            [TObj] TInt $ \optional ->
  notyet "buffered_output_length"
bf_read = Builtin "read" 0 (Just 2) [TObj, TAny] TStr $ \optional ->
  notyet "read"
bf_force_input = Builtin "force_input" 2 (Just 3)
                 [TObj, TStr, TAny] TAny $ \(Obj conn : Str line : optional) ->
  notyet "force_input"
bf_flush_input = Builtin "flush_input" 1 (Just 2)
                 [TObj, TAny] TAny $ \(Obj conn : optional) ->
  notyet "flush_input"
bf_output_delimiters = Builtin "output_delimiters" 1 (Just 1)
                       [TObj] TLst $ \[Obj player] ->
  notyet "output_delimiters"
bf_boot_player = Builtin "boot_player" 1 (Just 1) [TObj] TAny $ \[Obj player] ->
  notyet "boot_player"

bf_connection_name = Builtin "connection_name" 1 (Just 1)
                     [TObj] TStr $ \[Obj player] -> do
  checkPermission player
  (Str . Str.fromText) `liftM` getConnectionName player

bf_set_connection_option = Builtin "set_connection_option" 3 (Just 3)
                           [TObj, TStr, TAny]
                           TAny $ \[Obj conn, Str option, value] ->
  notyet "set_connection_option"
bf_connection_options = Builtin "connection_options" 1 (Just 1)
                        [TObj] TLst $ \[Obj conn] ->
  notyet "connection_options"
bf_connection_option = Builtin "connection_option" 2 (Just 2)
                       [TObj, TStr] TAny $ \[Obj conn, Str name] ->
  notyet "connection_option"

bf_open_network_connection = Builtin "open_network_connection" 2 (Just 3)
                             [TStr, TInt, TObj]
                             TObj $ \(Str host : Int port : optional) -> do
  let [Obj listener] = defaults optional [Obj systemObject]

  checkWizard
  connId <- openNetworkConnection
            (Str.toString host) (fromIntegral port) listener
  return (Obj connId)

bf_listen = Builtin "listen" 2 (Just 3)
            [TObj, TInt, TAny] TAny $ \(Obj object : Int point : optional) -> do
  let [print_messages] = booleanDefaults optional [False]

  checkWizard
  checkValid object

  canon <- listen (fromIntegral point) object print_messages
  return (Int $ fromIntegral canon)

bf_unlisten = Builtin "unlisten" 1 (Just 1) [TInt] TAny $ \[Int canon] ->
  checkWizard >> unlisten (fromIntegral canon) >> return zero

bf_listeners = Builtin "listeners" 0 (Just 0) [] TLst $ \[] ->
  (fromListBy formatListener . M.elems . listeners) `liftM` getWorld
