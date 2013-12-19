
{-# LANGUAGE OverloadedStrings #-}

module MOO.Object ( Object (..)
                  , Property (..)
                  , initObject
                  , initProperty
                  , getParent
                  , getChildren
                  , addChild
                  , builtinProperty
                  , isBuiltinProperty
                  , objectForMaybe
                  , setProperties
                  , setVerbs
                  , lookupPropertyRef
                  , lookupProperty
                  , lookupVerbRef
                  , lookupVerb
                  , replaceVerb
                  , addVerb
                  , deleteVerb
                  , definedProperties
                  , definedVerbs
                  ) where

import Control.Arrow (second)
import Control.Concurrent.STM
import Control.Monad (liftM)
import Data.HashMap.Strict (HashMap)
import Data.IntSet (IntSet)
import Data.Maybe
import Data.List (find)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.IntSet as IS

import MOO.Types
import MOO.Verb

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

instance Sizeable IntSet where
  storageBytes x = storageBytes () + storageBytes (0 :: Int) * IS.size x

instance (Sizeable k, Sizeable v) => Sizeable (HashMap k v) where
  storageBytes = HM.foldrWithKey bytes (storageBytes ())
    where bytes k v s = s + storageBytes k + storageBytes v

instance Sizeable (TVar a) where
  storageBytes _ = storageBytes ()

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

  , objectName       = T.empty
  , objectOwner      = -1
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

addChild :: Object -> ObjId -> Object
addChild obj childOid =
  obj { objectChildren = IS.insert childOid (objectChildren obj) }

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

  , propertyOwner     = -1
  , propertyPermR     = False
  , propertyPermW     = False
  , propertyPermC     = False
}

builtinProperty :: StrT -> Maybe (Object -> Value)
builtinProperty "name"       = Just (Str . objectName)
builtinProperty "owner"      = Just (Obj . objectOwner)
builtinProperty "location"   = Just (Obj . objectForMaybe . objectLocation)
builtinProperty "contents"   = Just (Lst . V.fromList
                                     . map Obj . IS.elems . objectContents)
builtinProperty "programmer" = Just (truthValue . objectProgrammer)
builtinProperty "wizard"     = Just (truthValue . objectWizard)
builtinProperty "r"          = Just (truthValue . objectPermR)
builtinProperty "w"          = Just (truthValue . objectPermW)
builtinProperty "f"          = Just (truthValue . objectPermF)
builtinProperty _            = Nothing

isBuiltinProperty :: StrT -> Bool
isBuiltinProperty = isJust . builtinProperty . T.toCaseFold

objectForMaybe :: Maybe ObjId -> ObjId
objectForMaybe (Just oid) = oid
objectForMaybe Nothing    = -1

setProperties :: [Property] -> Object -> IO Object
setProperties props obj = do
  propHash <- mkHash props
  return obj { objectProperties = propHash }
  where mkHash = liftM HM.fromList . mapM mkAssoc
        mkAssoc prop = do
          tvarProp <- newTVarIO prop
          return (propertyKey prop, tvarProp)

propertyKey :: Property -> StrT
propertyKey = T.toCaseFold . propertyName

setVerbs :: [Verb] -> Object -> IO Object
setVerbs verbs obj = do
  verbList <- mkList verbs
  return obj { objectVerbs = verbList }
  where mkList = mapM mkVerb
        mkVerb verb = do
          tvarVerb <- newTVarIO verb
          return (verbKey verb, tvarVerb)

verbKey :: Verb -> [StrT]
verbKey = T.words . T.toCaseFold . verbNames

lookupPropertyRef :: Object -> StrT -> Maybe (TVar Property)
lookupPropertyRef obj name = HM.lookup name (objectProperties obj)

lookupProperty :: Object -> StrT -> STM (Maybe Property)
lookupProperty obj name = maybe (return Nothing) (fmap Just . readTVar) $
                          lookupPropertyRef obj name

lookupVerbRef :: Object -> Value -> Maybe (Int, TVar Verb)
lookupVerbRef obj (Str name) =
  fmap (second snd) $ find matchVerb (zip [0..] $ objectVerbs obj)
  where matchVerb (_, (names, _)) = verbNameMatch (T.toCaseFold name) names
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

replaceVerb :: Object -> Int -> Verb -> Object
replaceVerb obj index verb =
  obj { objectVerbs = pre ++ [(verbKey verb, tvarVerb)] ++ tail post }
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
