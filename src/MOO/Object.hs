
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
                  , addContent
                  , deleteContent
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
                  , renumberObject
                  , renumberOwnership

                  -- * Special Object Numbers
                  , systemObject
                  , nothing
                  , ambiguousMatch
                  , failedMatch
                  ) where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Concurrent.STM (STM, TVar, newTVarIO, newTVar,
                               readTVar, writeTVar)
import Control.Monad ((>=>), forM_)
import Data.HashMap.Strict (HashMap)
import Data.IntSet (IntSet)
import Data.Maybe (isJust)
import Data.List (find)
import Prelude hiding (getContents)

import qualified Data.HashMap.Strict as HM
import qualified Data.IntSet as IS

import {-# SOURCE #-} MOO.Database
import MOO.Types
import MOO.Verb

import qualified MOO.String as Str

type VerbDef = ([StrT], TVar Verb)

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
  , objectVerbs      :: [VerbDef]
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

addContent :: ObjId -> Object -> STM Object
addContent oid obj =
  return obj { objectContents = IS.insert oid (objectContents obj) }

deleteContent :: ObjId -> Object -> STM Object
deleteContent oid obj =
  return obj { objectContents = IS.delete oid (objectContents obj) }

data Property = Property {
    propertyName      :: StrT
  , propertyValue     :: Maybe Value
  , propertyInherited :: Bool

  , propertyOwner     :: ObjId
  , propertyPermR     :: Bool
  , propertyPermW     :: Bool
  , propertyPermC     :: Bool
}

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

  where mkHash :: [Property] -> IO (HashMap StrT (TVar Property))
        mkHash = fmap HM.fromList . mapM mkAssoc

        mkAssoc :: Property -> IO (StrT, TVar Property)
        mkAssoc prop = do
          tvarProp <- newTVarIO prop
          return (propertyKey prop, tvarProp)

propertyKey :: Property -> StrT
propertyKey = propertyName

setVerbs :: [Verb] -> Object -> IO Object
setVerbs verbs obj = do
  verbList <- mkList verbs
  return obj { objectVerbs = verbList }

  where mkList :: [Verb] -> IO [VerbDef]
        mkList = mapM mkVerb

        mkVerb :: Verb -> IO VerbDef
        mkVerb verb = do
          verbRef <- newTVarIO verb
          return (verbKey verb, verbRef)

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

lookupVerbRef :: Bool -> Object -> Value -> Maybe (Int, TVar Verb)
lookupVerbRef numericStrings obj (Str name) =
  second snd <$> find matchVerb (zip [0..] $ objectVerbs obj)
  where matchVerb :: (Int, VerbDef) -> Bool
        matchVerb (i, (names, _)) = verbNameMatch name names ||
                                    (numericStrings && nameString == show i)
        nameString = Str.toString name :: String
lookupVerbRef _ obj (Int index)
  | index' < 1        = Nothing
  | index' > numVerbs = Nothing
  | otherwise         = Just (index'', snd $ verbs !! index'')
  where index'   = fromIntegral index :: Int
        index''  = index' - 1         :: Int
        verbs    = objectVerbs obj    :: [VerbDef]
        numVerbs = length verbs       :: Int
lookupVerbRef _ _ _ = Nothing

lookupVerb :: Bool -> Object -> Value -> STM (Maybe Verb)
lookupVerb numericStrings obj desc =
  maybe (return Nothing) (fmap Just . readTVar . snd) $
  lookupVerbRef numericStrings obj desc

replaceVerb :: Int -> Verb -> Object -> STM Object
replaceVerb index verb obj =
  return obj { objectVerbs = pre ++ [(verbKey verb, verbRef)] ++ tail post }
  where (pre, post) = splitAt index (objectVerbs obj) :: ([VerbDef], [VerbDef])
        verbRef     = snd (head post)                 :: TVar Verb

addVerb :: Verb -> Object -> STM Object
addVerb verb obj = do
  verbTVar <- newTVar verb
  return obj { objectVerbs = objectVerbs obj ++ [(verbKey verb, verbTVar)] }

deleteVerb :: Int -> Object -> STM Object
deleteVerb index obj = return obj { objectVerbs = pre ++ tail post }
  where (pre, post) = splitAt index (objectVerbs obj) :: ([VerbDef], [VerbDef])

definedProperties :: Object -> STM [StrT]
definedProperties obj = do
  props <- mapM readTVar $ HM.elems (objectProperties obj)
  return $ map propertyName $ filter (not . propertyInherited) props

definedVerbs :: Object -> STM [StrT]
definedVerbs obj = do
  verbs <- mapM (readTVar . snd) $ objectVerbs obj
  return $ map verbNames verbs

renumberObject :: Object -> ObjId -> ObjId -> Database -> STM ()
renumberObject obj old new db = do
  -- renumber parent/children
  case objectParent obj of
    Nothing     -> return ()
    Just parent -> modifyObject parent db $ deleteChild old >=> addChild new

  forM_ (getChildren obj) $ \child -> modifyObject child db $ \obj ->
    return obj { objectParent = Just new }

  -- renumber location/contents
  case objectLocation obj of
    Nothing    -> return ()
    Just place -> modifyObject place db $ deleteContent old >=> addContent new

  forM_ (getContents obj) $ \thing -> modifyObject thing db $ \obj ->
    return obj { objectLocation = Just new }

renumberOwnership :: ObjId -> ObjId -> Object -> STM (Maybe Object)
renumberOwnership old new obj = do
  -- renumber property ownerships
  forM_ (HM.elems $ objectProperties obj) $ \propRef -> do
    prop <- readTVar propRef
    case propertyOwner prop of
      owner | owner == new -> writeTVar propRef prop { propertyOwner = nothing }
            | owner == old -> writeTVar propRef prop { propertyOwner = new     }
      _ -> return ()

  -- renumber verb ownerships
  forM_ (map snd $ objectVerbs obj) $ \verbRef -> do
    verb <- readTVar verbRef
    case verbOwner verb of
      owner | owner == new -> writeTVar verbRef verb { verbOwner = nothing }
            | owner == old -> writeTVar verbRef verb { verbOwner = new     }
      _ -> return ()

  -- renumber object ownership
  return $ case objectOwner obj of
    owner | owner == new -> Just obj { objectOwner = nothing }
          | owner == old -> Just obj { objectOwner = new     }
    _ -> Nothing

-- | The system object (@#0@)
systemObject :: ObjId
systemObject = 0

-- | @$nothing@ (@#-1@)
nothing :: ObjId
nothing = -1

-- | @$ambiguous_match@ (@#-2@)
ambiguousMatch :: ObjId
ambiguousMatch = -2

-- | @$failed_match@ (@#-3@)
failedMatch :: ObjId
failedMatch = -3
