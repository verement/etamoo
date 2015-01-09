
{-# LANGUAGE OverloadedStrings #-}

module MOO.Object ( Object (..)
                  , Property (..)
                  , initObject
                  , initProperty
                  , getParent
                  , getChildren
                  , addChild
                  , deleteChild
                  , getContents
                  , builtinProperties
                  , builtinProperty
                  , isBuiltinProperty
                  , objectForMaybe
                  , setProperties
                  , setVerbs
                  , lookupPropertyRef
                  , lookupProperty
                  , addProperty
                  , addInheritedProperty
                  , deleteProperty
                  , lookupVerbRef
                  , lookupVerb
                  , replaceVerb
                  , addVerb
                  , deleteVerb
                  , definedProperties
                  , definedVerbs

                  -- * Special Object Numbers
                  , systemObject
                  , nothing
                  , ambiguousMatch
                  , failedMatch
                  ) where

import Control.Arrow (second)
import Control.Concurrent.STM (STM, TVar, newTVarIO, newTVar, readTVar)
import Control.Monad (liftM)
import Data.HashMap.Strict (HashMap)
import Data.IntSet (IntSet)
import Data.Maybe (isJust)
import Data.List (find)
import Prelude hiding (getContents)

import qualified Data.HashMap.Strict as HM
import qualified Data.IntSet as IS

import MOO.Types
import MOO.Verb

import qualified MOO.String as Str

data Object = Object {
  -- Attributes
    objectIsPlayer   :: Bool
  , objectParent     :: Maybe ObjId
  , objectChildren   :: IntSet

  -- Built-in properties
  , objectName       :: StrT
  , objectOwner      :: ObjId
  , objectLocation   :: Maybe ObjId
  , objectContents   :: IntSet
  , objectProgrammer :: Bool
  , objectWizard     :: Bool
  , objectPermR      :: Bool
  , objectPermW      :: Bool
  , objectPermF      :: Bool

  -- Definitions
  , objectProperties :: HashMap StrT (TVar Property)
  , objectVerbs      :: [([StrT], TVar Verb)]
}

instance Sizeable Object where
  -- this does not capture the size of defined properties or verbs, as these
  -- are tucked behind TVars and cannot be read outside the STM monad
  storageBytes obj =
    storageBytes (objectIsPlayer   obj) +
    storageBytes (objectParent     obj) +
    storageBytes (objectChildren   obj) +
    storageBytes (objectName       obj) +
    storageBytes (objectOwner      obj) +
    storageBytes (objectLocation   obj) +
    storageBytes (objectContents   obj) +
    storageBytes (objectProgrammer obj) +
    storageBytes (objectWizard     obj) +
    storageBytes (objectPermR      obj) +
    storageBytes (objectPermW      obj) +
    storageBytes (objectPermF      obj) +
    storageBytes (objectProperties obj) +
    storageBytes (objectVerbs      obj)

initObject = Object {
    objectIsPlayer   = False
  , objectParent     = Nothing
  , objectChildren   = IS.empty

  , objectName       = Str.empty
  , objectOwner      = nothing
  , objectLocation   = Nothing
  , objectContents   = IS.empty
  , objectProgrammer = False
  , objectWizard     = False
  , objectPermR      = False
  , objectPermW      = False
  , objectPermF      = False

  , objectProperties = HM.empty
  , objectVerbs      = []
}

instance Show Object where
  show _ = "<object>"

getParent :: Object -> ObjId
getParent = objectForMaybe . objectParent

getChildren :: Object -> [ObjId]
getChildren = IS.elems . objectChildren

addChild :: ObjId -> Object -> STM Object
addChild childOid obj =
  return obj { objectChildren = IS.insert childOid (objectChildren obj) }

deleteChild :: ObjId -> Object -> STM Object
deleteChild childOid obj =
  return obj { objectChildren = IS.delete childOid (objectChildren obj) }

getLocation :: Object -> ObjId
getLocation = objectForMaybe . objectLocation

getContents :: Object -> [ObjId]
getContents = IS.elems . objectContents

data Property = Property {
    propertyName      :: StrT
  , propertyValue     :: Maybe Value
  , propertyInherited :: Bool

  , propertyOwner     :: ObjId
  , propertyPermR     :: Bool
  , propertyPermW     :: Bool
  , propertyPermC     :: Bool
} deriving Show

instance Sizeable Property where
  storageBytes prop =
    storageBytes (propertyName      prop) +
    storageBytes (propertyValue     prop) +
    storageBytes (propertyInherited prop) +
    storageBytes (propertyOwner     prop) +
    storageBytes (propertyPermR     prop) +
    storageBytes (propertyPermW     prop) +
    storageBytes (propertyPermC     prop)

initProperty = Property {
    propertyName      = ""
  , propertyValue     = Nothing
  , propertyInherited = False

  , propertyOwner     = nothing
  , propertyPermR     = False
  , propertyPermW     = False
  , propertyPermC     = False
}

builtinProperties :: [Id]
builtinProperties = [ "name", "owner"
                    , "location", "contents"
                    , "programmer", "wizard"
                    , "r", "w", "f"
                    ]

builtinProperty :: StrT -> Maybe (Object -> Value)
builtinProperty "name"       = Just (Str . objectName)
builtinProperty "owner"      = Just (Obj . objectOwner)
builtinProperty "location"   = Just (Obj . getLocation)
builtinProperty "contents"   = Just (objectList . getContents)
builtinProperty "programmer" = Just (truthValue . objectProgrammer)
builtinProperty "wizard"     = Just (truthValue . objectWizard)
builtinProperty "r"          = Just (truthValue . objectPermR)
builtinProperty "w"          = Just (truthValue . objectPermW)
builtinProperty "f"          = Just (truthValue . objectPermF)
builtinProperty _            = Nothing

isBuiltinProperty :: StrT -> Bool
isBuiltinProperty = isJust . builtinProperty

objectForMaybe :: Maybe ObjId -> ObjId
objectForMaybe (Just oid) = oid
objectForMaybe Nothing    = nothing

setProperties :: [Property] -> Object -> IO Object
setProperties props obj = do
  propHash <- mkHash props
  return obj { objectProperties = propHash }
  where mkHash = liftM HM.fromList . mapM mkAssoc
        mkAssoc prop = do
          tvarProp <- newTVarIO prop
          return (propertyKey prop, tvarProp)

propertyKey :: Property -> StrT
propertyKey = propertyName

setVerbs :: [Verb] -> Object -> IO Object
setVerbs verbs obj = do
  verbList <- mkList verbs
  return obj { objectVerbs = verbList }
  where mkList = mapM mkVerb
        mkVerb verb = do
          tvarVerb <- newTVarIO verb
          return (verbKey verb, tvarVerb)

verbKey :: Verb -> [StrT]
verbKey = Str.words . verbNames

lookupPropertyRef :: Object -> StrT -> Maybe (TVar Property)
lookupPropertyRef obj name = HM.lookup name (objectProperties obj)

lookupProperty :: Object -> StrT -> STM (Maybe Property)
lookupProperty obj name = maybe (return Nothing) (fmap Just . readTVar) $
                          lookupPropertyRef obj name

addProperty :: Property -> Object -> STM Object
addProperty prop obj = do
  propTVar <- newTVar prop
  return obj { objectProperties =
                  HM.insert (propertyKey prop) propTVar $ objectProperties obj }

addInheritedProperty :: Property -> Object -> STM Object
addInheritedProperty prop obj =
  flip addProperty obj $ if propertyPermC prop
                         then prop' { propertyOwner = objectOwner obj }
                         else prop'
  where prop' = prop { propertyInherited = True, propertyValue = Nothing }

deleteProperty :: StrT -> Object -> STM Object
deleteProperty name obj =
  return obj { objectProperties = HM.delete name (objectProperties obj) }

lookupVerbRef :: Object -> Value -> Maybe (Int, TVar Verb)
lookupVerbRef obj (Str name) =
  fmap (second snd) $ find matchVerb (zip [0..] $ objectVerbs obj)
  where matchVerb (_, (names, _)) = verbNameMatch name names
lookupVerbRef obj (Int index)
  | index' < 1        = Nothing
  | index' > numVerbs = Nothing
  | otherwise         = Just (index'', snd $ verbs !! index'')
  where index'   = fromIntegral index
        index''  = index' - 1
        verbs    = objectVerbs obj
        numVerbs = length verbs
lookupVerbRef _ _ = Nothing

lookupVerb :: Object -> Value -> STM (Maybe Verb)
lookupVerb obj desc = maybe (return Nothing) (fmap Just . readTVar . snd) $
                      lookupVerbRef obj desc

replaceVerb :: Int -> Verb -> Object -> STM Object
replaceVerb index verb obj =
  return obj { objectVerbs = pre ++ [(verbKey verb, tvarVerb)] ++ tail post }
  where (pre, post) = splitAt index (objectVerbs obj)
        tvarVerb = snd $ head post

addVerb :: Verb -> Object -> STM Object
addVerb verb obj = do
  verbTVar <- newTVar verb
  return obj { objectVerbs = objectVerbs obj ++ [(verbKey verb, verbTVar)] }

deleteVerb :: Int -> Object -> STM Object
deleteVerb index obj = return obj { objectVerbs = verbs }
  where verbs = before ++ tail after
        (before, after) = splitAt index (objectVerbs obj)

definedProperties :: Object -> STM [StrT]
definedProperties obj = do
  props <- mapM readTVar $ HM.elems (objectProperties obj)
  return $ map propertyName $ filter (not . propertyInherited) props

definedVerbs :: Object -> STM [StrT]
definedVerbs obj = do
  verbs <- mapM (readTVar . snd) $ objectVerbs obj
  return $ map verbNames verbs

-- | Special object numbers
systemObject, nothing, ambiguousMatch, failedMatch :: ObjId
systemObject   =  0
nothing        = -1
ambiguousMatch = -2
failedMatch    = -3
