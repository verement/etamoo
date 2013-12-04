
{-# LANGUAGE OverloadedStrings #-}

module MOO.Builtins.Objects ( builtins ) where

import Control.Monad (unless)

import qualified Data.Vector as V

import MOO.Builtins.Common
import MOO.Database
import MOO.Types
import MOO.Task

-- 4.4.3 Manipulating Objects

builtins :: [BuiltinSpec]
builtins = [
    ("create"        , (bf_create        , Info 1 (Just 2) [TObj, TObj] TObj))
  , ("chparent"      , (bf_chparent      , Info 2 (Just 2) [TObj, TObj] TAny))
  , ("valid"         , (bf_valid         , Info 1 (Just 1) [TObj]       TInt))
  , ("parent"        , (bf_parent        , Info 1 (Just 1) [TObj]       TObj))
  , ("children"      , (bf_children      , Info 1 (Just 1) [TObj]       TLst))
  , ("recycle"       , (bf_recycle       , Info 1 (Just 1) [TObj]       TAny))
  , ("object_bytes"  , (bf_object_bytes  , Info 1 (Just 1) [TObj]       TInt))
  , ("max_object"    , (bf_max_object    , Info 0 (Just 0) []           TObj))

  , ("move"          , (bf_move          , Info 2 (Just 2) [TObj, TObj] TAny))
  , ("properties"    , (bf_properties    , Info 1 (Just 1) [TObj]       TLst))
  , ("property_info" , (bf_property_info , Info 2 (Just 2) [TObj, TStr] TLst))
  , ("set_property_info",
                    (bf_set_property_info, Info 3 (Just 3) [TObj, TStr,
                                                            TLst]       TAny))
  , ("add_property"  , (bf_add_property  , Info 4 (Just 4) [TObj, TStr,
                                                            TAny, TLst] TAny))
  , ("delete_property",
                      (bf_delete_property, Info 2 (Just 2) [TObj, TStr] TAny))
  , ("is_clear_property",
                    (bf_is_clear_property, Info 2 (Just 2) [TObj, TStr] TInt))
  , ("clear_property", (bf_clear_property, Info 2 (Just 2) [TObj, TStr] TAny))

  , ("verbs"         , (bf_verbs         , Info 1 (Just 1) [TObj]       TLst))
  , ("verb_info"     , (bf_verb_info     , Info 2 (Just 2) [TObj, TStr] TLst))
  , ("set_verb_info" , (bf_set_verb_info , Info 3 (Just 3) [TObj, TStr,
                                                            TLst]       TAny))
  , ("verb_args"     , (bf_verb_args     , Info 2 (Just 2) [TObj, TStr] TLst))
  , ("set_verb_args" , (bf_set_verb_args , Info 3 (Just 3) [TObj, TStr,
                                                            TLst]       TAny))
  , ("add_verb"      , (bf_add_verb      , Info 3 (Just 3) [TObj, TLst,
                                                            TLst]       TInt))
  , ("delete_verb"   , (bf_delete_verb   , Info 2 (Just 2) [TObj, TStr] TAny))
  , ("verb_code"     , (bf_verb_code     , Info 2 (Just 4) [TObj, TStr,
                                                            TAny, TAny] TLst))
  , ("set_verb_code" , (bf_set_verb_code , Info 3 (Just 3) [TObj, TStr,
                                                            TLst]       TLst))
  , ("disassemble"   , (bf_disassemble   , Info 2 (Just 2) [TObj, TStr] TLst))

  , ("players"       , (bf_players       , Info 0 (Just 0) []           TLst))
  , ("is_player"     , (bf_is_player     , Info 1 (Just 1) [TObj]       TInt))
  , ("set_player_flag",
                      (bf_set_player_flag, Info 2 (Just 2) [TObj, TAny] TAny))
  ]

-- 4.4.3.1 Fundamental Operations on Objects

bf_create (Obj parent : owner) = notyet
bf_chparent [Obj object, Obj new_parent] = notyet

bf_valid [Obj object] = fmap truthValue $ isValid object =<< getDatabase

bf_parent [Obj object] = notyet
bf_children [Obj object] = notyet
bf_recycle [Obj object] = notyet
bf_object_bytes [Obj object] = notyet

bf_max_object [] = fmap (Obj . maxObject) getDatabase

-- 4.4.3.2 Object Movement

bf_move [Obj what, Obj where_] = notyet

-- 4.4.3.3 Operations on Properties

bf_properties [Obj object] = notyet
bf_property_info [Obj object, Str prop_name] = notyet
bf_set_property_info [Obj object, Str prop_name, Lst info] = notyet
bf_add_property [Obj object, Str prop_name, value, Lst info] = notyet
bf_delete_property [Obj object, Str prop_name] = notyet
bf_is_clear_property [Obj object, Str prop_name] = notyet
bf_clear_property [Obj object, Str prop_name] = notyet

-- 4.4.3.4 Operations on Verbs

bf_verbs [Obj object] = notyet
bf_verb_info [Obj object, Str verb_desc] = notyet
bf_set_verb_info [Obj object, Str verb_desc, Lst info] = notyet
bf_verb_args [Obj object, Str verb_desc] = notyet
bf_set_verb_args [Obj object, Str verb_desc, Lst args] = notyet
bf_add_verb [Obj object, Lst info, Lst args] = notyet
bf_delete_verb [Obj object, Str verb_desc] = notyet
bf_verb_code (Obj object : Str verb_desc : options) = notyet
bf_set_verb_code [Obj object, Str verb_desc, Lst code] = notyet
bf_disassemble [Obj object, Str verb_desc] = notyet

-- 4.4.3.5 Operations on Player Objects

bf_players [] = fmap (Lst . V.fromList . map Obj . allPlayers) getDatabase

bf_is_player [Obj object] = do
  db <- getDatabase
  valid <- isValid object db
  unless valid $ raise E_INVARG
  return (truthValue $ isPlayer object db)

bf_set_player_flag [Obj object, value] = notyet
