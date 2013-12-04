
{-# LANGUAGE OverloadedStrings #-}

module MOO.Object ( Object (..)
                  , initObject
                  , getParent
                  , getChildren
                  , builtinProperty
                  ) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.IntSet (IntSet)
import Data.Maybe

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.IntSet as IS

import MOO.Types
import MOO.Verb
import MOO.Types

data Object = Object {
  -- Attributes
    parent         :: Maybe ObjId
  , children       :: IntSet

  -- Built-in properties
  , slotName       :: StrT
  , slotOwner      :: ObjT
  , slotLocation   :: Maybe ObjId
  , slotContents   :: IntSet
  , slotProgrammer :: Bool
  , slotWizard     :: Bool
  , slotR          :: Bool
  , slotW          :: Bool
  , slotF          :: Bool

  -- Definitions
  , properties     :: HashMap StrT Property
  , verbs          :: [Verb]
}

initObject = Object {
    parent         = Nothing
  , children       = IS.empty

  , slotName       = T.empty
  , slotOwner      = -1
  , slotLocation   = Nothing
  , slotContents   = IS.empty
  , slotProgrammer = False
  , slotWizard     = False
  , slotR          = False
  , slotW          = False
  , slotF          = False

  , properties     = HM.empty
  , verbs          = []
}

instance Show Object where
  show _ = "<object>"

getParent :: Object -> ObjId
getParent = objectForMaybe . parent

getChildren :: Object -> [ObjId]
getChildren = IS.elems . children

data Property = Property {
    propName      :: StrT
  , propValue     :: Maybe Value
  , propInherited :: Bool

  , propOwner     :: ObjT
  , propPermR     :: Bool
  , propPermW     :: Bool
  , propPermC     :: Bool
} deriving Show

{-
getProperty :: Object -> Text -> Maybe Property
getProperty obj pn = HM.lookup (T.toCaseFold pn) (properties obj)

getPropValue :: Object -> Text -> Maybe Value
getPropValue obj pn = get (T.toCaseFold pn) obj
  where get pn obj = do
          p <- HM.lookup pn $ properties obj
          case propValue p of
            Just v  -> return v
            Nothing -> parent obj >>= get pn
-}

builtinProperty :: StrT -> Maybe (Object -> Value)
builtinProperty "name"       = Just (Str        . slotName)
builtinProperty "owner"      = Just (Obj        . slotOwner)
builtinProperty "location"   = Just (Obj        . objectForMaybe . slotLocation)
builtinProperty "contents"   = Just (Lst . V.fromList .
                                     map Obj . IS.elems . slotContents)
builtinProperty "programmer" = Just (truthValue . slotProgrammer)
builtinProperty "wizard"     = Just (truthValue . slotWizard)
builtinProperty "r"          = Just (truthValue . slotR)
builtinProperty "w"          = Just (truthValue . slotW)
builtinProperty "f"          = Just (truthValue . slotF)
builtinProperty _            = Nothing

isBuiltinProperty = isJust . builtinProperty . T.toCaseFold

objectForMaybe :: Maybe ObjId -> ObjId
objectForMaybe (Just oid) = oid
objectForMaybe Nothing    = -1
