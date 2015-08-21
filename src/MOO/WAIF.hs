
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module MOO.WAIF (
    WAIF
  , newWaif

  , waifClass

  , fetchWaifProperty
  , storeWaifProperty

  , callWaifVerb
  , mangleWaifVerbName
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad ((>=>), unless)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Database.VCache (PVar, VRef, VCacheable(put, get), VTx, deref, vref,
                        pvar_space, newPVar, readPVar, modifyPVar,
                        unsafePVarAddr, VSpace)

import qualified Data.HashMap.Lazy as HM

import MOO.Object
import MOO.Task
import MOO.Types
import MOO.Util

import qualified MOO.String as Str

type PropertyMap = VHashMap StrT (VRef Value)

data WAIF = WAIF {
    waifClass :: ObjId
  , waifOwner :: ObjId
  , waifData  :: PVar PropertyMap
  } deriving (Eq, Typeable)

instance Show WAIF where
  showsPrec _ w = showString "[[WAIF@" . shows (unsafePVarAddr $ waifData w)
                . showString ": class = #" . shows (waifClass w)
                . showString ", owner = #" . shows (waifOwner w)
                . showString "]]"

instance VCacheable WAIF where
  put waif = do
    put $ waifClass waif
    put $ waifOwner waif
    put $ waifData  waif

  get = WAIF <$> get <*> get <*> get

newWaif :: ObjId -> ObjId -> VTx WAIF
newWaif waifClass waifOwner = do
  waifData <- newPVar (VHashMap HM.empty)

  return WAIF { waifClass = waifClass
              , waifOwner = waifOwner
              , waifData  = waifData
              }

waifClassObject :: WAIF -> MOO Object
waifClassObject = getObject . waifClass >=> maybe (raise E_INVIND) return

prefix = ":" :: StrT

waifProperty :: StrT -> Object -> MOO Property
waifProperty name obj = getProperty obj (prefix <> name)

waifPropertyOwner :: WAIF -> Property -> ObjId
waifPropertyOwner waif prop
  | propertyPermC prop = waifOwner waif
  | otherwise          = propertyOwner prop

fetchWaifProperty :: WAIF -> StrT -> MOO Value
fetchWaifProperty waif "class" = return (Obj $ waifClass waif)
fetchWaifProperty waif "owner" = return (Obj $ waifOwner waif)
fetchWaifProperty waif name = do
  classObj <- waifClassObject waif
  prop <- waifProperty name classObj
  unless (propertyPermR prop) $ checkPermission (waifPropertyOwner waif prop)

  maybe (search classObj prop) (return . deref) .
    HM.lookup name . unVHashMap =<< liftVTx (readPVar $ waifData waif)

  where search :: Object -> Property -> MOO Value
        search obj prop = case propertyValue prop of
          Just value -> return (deref value)
          Nothing    -> do
            let name = propertyName prop
            maybeParent <- maybe (return Nothing) getObject (objectParent obj)
            case maybeParent of
              Just parent -> getProperty parent name >>= search parent
              Nothing     -> error $
                "No inherited value for property " ++ Str.toString name

storeWaifProperty :: WAIF -> StrT -> Value -> MOO Value
storeWaifProperty _ "class" _ = raise E_PERM
storeWaifProperty _ "owner" _ = raise E_PERM
storeWaifProperty waif name value = do
  prop <- waifProperty name =<< waifClassObject waif
  unless (propertyPermW prop) $ checkPermission (waifPropertyOwner waif prop)

  let var    = waifData waif  :: PVar PropertyMap
      vspace = pvar_space var :: VSpace

  liftVTx $ modifyPVar var $
    VHashMap . HM.insert name (vref vspace value) . unVHashMap

  return value

callWaifVerb :: Value -> WAIF -> StrT -> [Value] -> MOO Value
callWaifVerb this waif name = callVerb this (waifClass waif) (prefix <> name)

mangleWaifVerbName :: StrT -> StrT
mangleWaifVerbName name
  | prefix `Str.isPrefixOf` name = Str.drop (Str.length prefix) name
  | otherwise                    = name
