
{-# LANGUAGE OverloadedStrings #-}

module MOO.Object ( Object (..)
                  , initObject
                  , maybeObject
                  ) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Maybe

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

import MOO.Types
import MOO.Verb

data Object = Object {
  -- Attributes
    parent         :: Maybe ObjId
  , children       :: [ObjId]

  -- Built-in properties
  , slotName       :: StrT
  , slotOwner      :: ObjT
  , slotLocation   :: Maybe ObjId
  , slotContents   :: LstT
  , slotProgrammer :: Bool
  , slotWizard     :: Bool
  , slotR          :: Bool
  , slotW          :: Bool
  , slotF          :: Bool

  -- Definitions
  , properties     :: HashMap Text Property
  , verbs          :: [Verb]
}

initObject = Object {
    parent         = Nothing
  , children       = []

  , slotName       = T.empty
  , slotOwner      = -1
  , slotLocation   = Nothing
  , slotContents   = V.empty
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

data Property = Property {
    propName      :: StrT
  , propValue     :: Maybe Value
  , propInherited :: Bool

  , propOwner     :: ObjT
  , propPermR     :: Bool
  , propPermW     :: Bool
  , propPermC     :: Bool
} deriving Show

getProperty :: Object -> Text -> Maybe Property
getProperty obj pn = HM.lookup (T.toCaseFold pn) (properties obj)

{-
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
builtinProperty "contents"   = Just (Lst        . slotContents)
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

maybeObject :: ObjId -> Maybe ObjId
maybeObject oid
  | oid >=  0 = Just oid
  | otherwise = Nothing
