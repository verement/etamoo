
{-# LANGUAGE OverloadedStrings #-}

module MOO.Builtins.Network ( builtins ) where

import Control.Concurrent.STM (readTVar)
import Control.Monad (liftM, (<=<))
import Control.Monad.State (gets)
import Data.Time (UTCTime, diffUTCTime)

import qualified Data.Map as M

import MOO.Builtins.Common
import MOO.Network
import MOO.Connection
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

  objects <- withConnections $ return . M.keys
  return $ objectList $ if include_all then objects else filter (>= 0) objects

secondsSince :: UTCTime -> MOO Value
secondsSince utcTime = do
  now <- gets startTime
  return (Int $ floor $ now `diffUTCTime` utcTime)

bf_connected_seconds = Builtin "connected_seconds" 1 (Just 1)
                       [TObj] TInt $ \[Obj player] ->
  withConnection player $ maybe (raise E_INVARG) secondsSince <=<
    liftSTM . readTVar . connectionConnectedTime

bf_idle_seconds = Builtin "idle_seconds" 1 (Just 1)
                  [TObj] TInt $ \[Obj player] ->
  withConnection player $ secondsSince <=<
    liftSTM . readTVar . connectionActivityTime

bf_notify = Builtin "notify" 2 (Just 3)
            [TObj, TStr, TAny] TAny $ \(Obj conn : Str string : optional) -> do
  let [no_flush] = booleanDefaults optional [False]

  checkPermission conn
  truthValue `liftM` notify' no_flush conn string

bf_buffered_output_length = Builtin "buffered_output_length" 0 (Just 1)
                            [TObj] TInt $ \optional ->
  notyet "buffered_output_length"

bf_read = Builtin "read" 0 (Just 2) [TObj, TAny] TAny $ \optional ->
  notyet "read"

bf_force_input = Builtin "force_input" 2 (Just 3) [TObj, TStr, TAny]
                 TAny $ \(Obj conn : Str line : optional) -> do
  let [at_front] = booleanDefaults optional [False]

  checkPermission conn
  forceInput at_front conn line
  return zero

bf_flush_input = Builtin "flush_input" 1 (Just 2)
                 [TObj, TAny] TAny $ \(Obj conn : optional) ->
  notyet "flush_input"

bf_output_delimiters = Builtin "output_delimiters" 1 (Just 1)
                       [TObj] TLst $ \[Obj player] -> do
  checkPermission player
  (prefix, suffix) <- withConnection player $
                      liftSTM . readTVar . connectionOutputDelimiters
  return $ stringList [Str.fromText prefix, Str.fromText suffix]

bf_boot_player = Builtin "boot_player" 1 (Just 1) [TObj] TAny $ \[Obj player] ->
  checkPermission player >> bootPlayer player >> return zero

bf_connection_name = Builtin "connection_name" 1 (Just 1)
                     [TObj] TStr $ \[Obj player] -> do
  checkPermission player
  (Str . Str.fromString) `liftM`
    withConnection player (liftSTM . connectionName)

bf_set_connection_option = Builtin "set_connection_option" 3 (Just 3)
                           [TObj, TStr, TAny]
                           TAny $ \[Obj conn, Str option, value] -> do
  checkPermission conn
  setConnectionOption conn (toId option) value
  return zero

bf_connection_options = Builtin "connection_options" 1 (Just 1)
                        [TObj] TLst $ \[Obj conn] -> do
  checkPermission conn
  (fromListBy pair . M.toList) `liftM` getConnectionOptions conn

  where pair (k, v) = fromList [Str $ fromId k, v]

bf_connection_option = Builtin "connection_option" 2 (Just 2)
                       [TObj, TStr] TAny $ \[Obj conn, Str name] -> do
  checkPermission conn
  getConnectionOptions conn >>=
    maybe (raise E_INVARG) return . M.lookup (toId name)

bf_open_network_connection = Builtin "open_network_connection" 2 (Just 3)
                             [TStr, TInt, TObj]
                             TObj $ \(Str host : Int port : optional) -> do
  let [Obj listener] = defaults optional [Obj systemObject]

  notyet "open_network_connection"
{-
  checkWizard
  connId <- openNetworkConnection
            (Str.toString host) (fromIntegral port) listener
  return (Obj connId)
-}

bf_listen = Builtin "listen" 2 (Just 3)
            [TObj, TAny, TAny] TAny $ \(Obj object : point : optional) -> do
  let [print_messages] = booleanDefaults optional [False]

  checkWizard
  checkValid object

  point <- value2point point
  point2value `liftM` listen object point print_messages

bf_unlisten = Builtin "unlisten" 1 (Just 1) [TAny] TAny $ \[canon] -> do
  checkWizard

  unlisten =<< value2point canon
  return zero

bf_listeners = Builtin "listeners" 0 (Just 0) [] TLst $ \[] ->
  (fromListBy formatListener . M.elems . listeners) `liftM` getWorld

  where formatListener Listener { listenerObject        = object
                                , listenerPoint         = point
                                , listenerPrintMessages = printMessages } =
          fromList [Obj object, point2value point, truthValue printMessages]
