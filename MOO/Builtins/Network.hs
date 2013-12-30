
{-# LANGUAGE OverloadedStrings #-}

module MOO.Builtins.Network ( builtins ) where

import MOO.Types
import MOO.Task
import MOO.Network
import MOO.Builtins.Common

import qualified Data.Text as T

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- 4.4.4 Operations on Network Connections

builtins :: [BuiltinSpec]
builtins = [
    ("connected_players",
                    (bf_connected_players, Info 0 (Just 1) [TAny]       TLst))
  , ("connected_seconds",
                    (bf_connected_seconds, Info 1 (Just 1) [TObj]       TInt))
  , ("idle_seconds"  , (bf_idle_seconds  , Info 1 (Just 1) [TObj]       TInt))
  , ("notify"        , (bf_notify        , Info 2 (Just 3) [TObj, TStr,
                                                            TAny]       TAny))
  , ("buffered_output_length",
               (bf_buffered_output_length, Info 0 (Just 1) [TObj]       TInt))
  , ("read"          , (bf_read          , Info 0 (Just 2) [TObj, TAny] TStr))
  , ("force_input"   , (bf_force_input   , Info 2 (Just 3) [TObj, TStr,
                                                            TAny]       TAny))
  , ("flush_input"   , (bf_flush_input   , Info 1 (Just 2) [TObj, TAny] TAny))
  , ("output_delimiters",
                    (bf_output_delimiters, Info 1 (Just 1) [TObj]       TLst))
  , ("boot_player"   , (bf_boot_player   , Info 1 (Just 1) [TObj]       TAny))
  , ("connection_name",
                      (bf_connection_name, Info 1 (Just 1) [TObj]       TStr))
  , ("set_connection_option",
                (bf_set_connection_option, Info 3 (Just 3) [TObj, TStr,
                                                            TAny]       TAny))
  , ("connection_options",
                   (bf_connection_options, Info 1 (Just 1) [TObj]       TLst))
  , ("connection_option",
                    (bf_connection_option, Info 2 (Just 2) [TObj, TStr] TAny))
  , ("open_network_connection",
              (bf_open_network_connection, Info 0 Nothing  []           TObj))
  , ("listen"        , (bf_listen        , Info 2 (Just 3) [TObj, TAny,
                                                            TAny]       TAny))
  , ("unlisten"      , (bf_unlisten      , Info 1 (Just 1) [TAny]       TAny))
  , ("listeners"     , (bf_listeners     , Info 0 (Just 0) []           TLst))
  ]

bf_connected_players optional = notyet "connected_players"
bf_connected_seconds [Obj player] = notyet "connected_seconds"
bf_idle_seconds [Obj player] = notyet "idle_seconds"

bf_notify (Obj conn : Str string : optional) = do
  notify conn string
  return $ truthValue True
  where [no_flush] = booleanDefaults optional [False]

bf_buffered_output_length optional = notyet "buffered_output_length"
bf_read optional = notyet "read"
bf_force_input (Obj conn : Str line : optional) = notyet "force_input"
bf_flush_input (Obj conn : optional) = notyet "flush_input"
bf_output_delimiters [Obj player] = notyet "output_delimiters"
bf_boot_player [Obj player] = notyet "boot_player"
bf_connection_name [Obj player] = notyet "connection_name"
bf_set_connection_option [Obj conn, Str option, value] =
  notyet "set_connection_option"
bf_connection_options [Obj conn] = notyet "connection_options"
bf_connection_option [Obj conn, Str name] = notyet "connection_option"
bf_open_network_connection args = notyet "open_network_connection"
bf_listen (Obj object : args) = notyet "listen"
bf_unlisten [canon] = notyet "unlisten"
bf_listeners [] = notyet "listeners"
