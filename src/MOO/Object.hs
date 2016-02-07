
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

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

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (second)
import Control.Monad ((>=>), forM_)
import Data.HashMap.Strict (HashMap)
import Data.IntSet (IntSet)
import Data.List (find)
import Data.Maybe (isJust)
import Data.Typeable (Typeable)
import Database.VCache (VCacheable(put, get), VSpace, VTx, getVTxSpace,
                        VRef, vref, vref', deref, deref',
                        PVar, newPVarIO, newPVar, readPVar, writePVar)
import Prelude hiding (getContents)

import qualified Data.HashMap.Strict as HM
import qualified Data.IntSet as IS

import {-# SOURCE #-} MOO.Database
import MOO.Types
import MOO.Util
import MOO.Verb

import qualified MOO.String as Str

type VerbDef = ([StrT], PVar (VRef Verb))

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
  , objectProperties :: HashMap StrT (PVar (VRef Property))
  , objectVerbs      :: [VerbDef]
} deriving Typeable

instance VCacheable Object where
  put obj = do
    put $ objectIsPlayer   obj
    put $ objectParent     obj
    put $ VIntSet (objectChildren obj)
    put $ objectName       obj
    put $ objectOwner      obj
    put $ objectLocation   obj
    put $ VIntSet (objectContents obj)
    put $ objectProgrammer obj
    put $ objectWizard     obj
    put $ objectPermR      obj
    put $ objectPermW      obj
    put $ objectPermF      obj
    put $ VHashMap (objectProperties obj)
    put $ objectVerbs      obj

  get = Object <$> get <*> get <*> (unVIntSet <$> get)
               <*> get <*> get <*> get <*> (unVIntSet <$> get)
               <*> get <*> get <*> get <*> get <*> get
               <*> (unVHashMap <$> get) <*> get

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

getParent :: Object -> ObjId
getParent = objectForMaybe . objectParent

getChildren :: Object -> [ObjId]
getChildren = IS.elems . objectChildren

addChild :: ObjId -> Object -> VTx Object
addChild childOid obj =
  return obj { objectChildren = IS.insert childOid (objectChildren obj) }

deleteChild :: ObjId -> Object -> VTx Object
deleteChild childOid obj =
  return obj { objectChildren = IS.delete childOid (objectChildren obj) }

getLocation :: Object -> ObjId
getLocation = objectForMaybe . objectLocation

getContents :: Object -> [ObjId]
getContents = IS.elems . objectContents

addContent :: ObjId -> Object -> VTx Object
addContent oid obj =
  return obj { objectContents = IS.insert oid (objectContents obj) }

deleteContent :: ObjId -> Object -> VTx Object
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
} deriving Typeable

instance VCacheable Property where
  put prop = do
    put $ propertyName      prop
    put $ propertyValue     prop
    put $ propertyInherited prop
    put $ propertyOwner     prop
    put $ propertyPermR     prop
    put $ propertyPermW     prop
    put $ propertyPermC     prop

  get = Property <$> get <*> get <*> get
                 <*> get <*> get <*> get <*> get

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

setProperties :: VSpace -> [Property] -> Object -> IO Object
setProperties vspace props obj = do
  propHash <- mkHash props
  return obj { objectProperties = propHash }

  where mkHash :: [Property] -> IO (HashMap StrT (PVar (VRef Property)))
        mkHash = fmap HM.fromList . mapM mkAssoc

        mkAssoc :: Property -> IO (StrT, PVar (VRef Property))
        mkAssoc prop = do
          tvarProp <- newPVarIO vspace (vref' vspace prop)
          return (propertyKey prop, tvarProp)

propertyKey :: Property -> StrT
propertyKey = propertyName

setVerbs :: VSpace -> [Verb] -> Object -> IO Object
setVerbs vspace verbs obj = do
  verbList <- mkList verbs
  return obj { objectVerbs = verbList }

  where mkList :: [Verb] -> IO [VerbDef]
        mkList = mapM mkVerb

        mkVerb :: Verb -> IO VerbDef
        mkVerb verb = do
          verbRef <- newPVarIO vspace (vref vspace verb)
          return (verbKey verb, verbRef)

verbKey :: Verb -> [StrT]
verbKey = Str.words . verbNames

lookupPropertyRef :: Object -> StrT -> Maybe (PVar (VRef Property))
lookupPropertyRef obj name = HM.lookup name (objectProperties obj)

lookupProperty :: Object -> StrT -> VTx (Maybe Property)
lookupProperty obj name = maybe (return Nothing)
  (fmap (Just . deref) . readPVar) $ lookupPropertyRef obj name

addProperty :: Property -> Object -> VTx Object
addProperty prop obj = do
  vspace <- getVTxSpace
  propPVar <- newPVar (vref vspace prop)
  return obj { objectProperties =
                 HM.insert (propertyKey prop) propPVar $ objectProperties obj }

addInheritedProperty :: Property -> Object -> VTx Object
addInheritedProperty prop obj =
  flip addProperty obj $ if propertyPermC prop
                         then prop' { propertyOwner = objectOwner obj }
                         else prop'
  where prop' = prop { propertyInherited = True, propertyValue = Nothing }

deleteProperty :: StrT -> Object -> VTx Object
deleteProperty name obj =
  return obj { objectProperties = HM.delete name (objectProperties obj) }

lookupVerbRef :: Bool -> Object -> Value -> Maybe (Int, PVar (VRef Verb))
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

lookupVerb :: Bool -> Object -> Value -> VTx (Maybe Verb)
lookupVerb numericStrings obj desc =
  maybe (return Nothing) (fmap (Just . deref) . readPVar . snd) $
  lookupVerbRef numericStrings obj desc

replaceVerb :: Int -> Verb -> Object -> VTx Object
replaceVerb index verb obj =
  return obj { objectVerbs = pre ++ [(verbKey verb, verbRef)] ++ tail post }
  where (pre, post) = splitAt index (objectVerbs obj) :: ([VerbDef], [VerbDef])
        verbRef     = snd (head post)                 :: PVar (VRef Verb)

addVerb :: Verb -> Object -> VTx Object
addVerb verb obj = do
  vspace <- getVTxSpace
  verbPVar <- newPVar (vref vspace verb)
  return obj { objectVerbs = objectVerbs obj ++ [(verbKey verb, verbPVar)] }

deleteVerb :: Int -> Object -> VTx Object
deleteVerb index obj = return obj { objectVerbs = pre ++ tail post }
  where (pre, post) = splitAt index (objectVerbs obj) :: ([VerbDef], [VerbDef])

definedProperties :: Object -> VTx [StrT]
definedProperties obj = do
  props <- mapM (fmap deref . readPVar) $ HM.elems (objectProperties obj)
  return $ map propertyName $ filter (not . propertyInherited) props

definedVerbs :: Object -> VTx [StrT]
definedVerbs obj = do
  verbs <- mapM (fmap deref . readPVar . snd) $ objectVerbs obj
  return $ map verbNames verbs

renumberObject :: Object -> ObjId -> ObjId -> Database -> VTx ()
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

renumberOwnership :: ObjId -> ObjId -> Object -> VTx (Maybe Object)
renumberOwnership old new obj = do
  vspace <- getVTxSpace

  -- renumber property ownerships
  forM_ (HM.elems $ objectProperties obj) $ \propRef -> do
    prop <- deref' <$> readPVar propRef
    case propertyOwner prop of
      owner | owner == new -> writePVar propRef $
              vref' vspace prop { propertyOwner = nothing }
            | owner == old -> writePVar propRef $
              vref' vspace prop { propertyOwner = new     }
      _ -> return ()

  -- renumber verb ownerships
  forM_ (map snd $ objectVerbs obj) $ \verbRef -> do
    verb <- deref' <$> readPVar verbRef
    case verbOwner verb of
      owner | owner == new ->
                writePVar verbRef $ vref' vspace verb { verbOwner = nothing }
            | owner == old ->
                writePVar verbRef $ vref' vspace verb { verbOwner = new     }
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
