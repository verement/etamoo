
{-# LANGUAGE DeriveDataTypeable #-}

module MOO.Util (
    ctime
  , storageBytes
  , VIntSet(..)
  , VHashMap(..)
  , VUTCTime(..)
  , VVector(..)
  , VVersion(..)
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.IntSet (IntSet)
import Data.Ratio (numerator, denominator, (%))
import Data.Time (UTCTime(..), Day(..), DiffTime, utcToLocalZonedTime,
                  formatTime, defaultTimeLocale)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Data.Version (Version(..))
import Database.VCache (VCacheable(put, get), putVarNat, getVarNat,
                        VRef, unsafeVRefEncoding)

import qualified Data.HashMap.Strict as HM
import qualified Data.IntSet as IS
import qualified Data.Vector as V

-- | Return a string representation of a time value in standard MOO format.
ctime :: UTCTime -> IO String
ctime time = formatTime defaultTimeLocale "%a %b %_d %T %Y %Z" <$>
             utcToLocalZonedTime time

-- | Return the number of bytes used to store a referenced value in the
-- persistence layer.
storageBytes :: VRef a -> IO Int
storageBytes ref = unsafeVRefEncoding ref $ const return

newtype VIntSet = VIntSet { unVIntSet :: IntSet } deriving Typeable

instance VCacheable VIntSet where
  put = put . IS.toList . unVIntSet
  get = VIntSet . IS.fromList <$> get

newtype VHashMap k v = VHashMap { unVHashMap :: HashMap k v } deriving Typeable

instance (Eq k, Hashable k,
          VCacheable k, VCacheable v) => VCacheable (VHashMap k v) where
  put = put . HM.toList . unVHashMap
  get = VHashMap . HM.fromList <$> get

newtype VUTCTime = VUTCTime { unVUTCTime :: UTCTime } deriving Typeable

instance VCacheable VUTCTime where
  put (VUTCTime time) = do
    put $ VDay      (utctDay     time)
    put $ VDiffTime (utctDayTime time)

  get = VUTCTime <$> (UTCTime <$> (unVDay <$> get) <*> (unVDiffTime <$> get))

newtype VDay = VDay { unVDay :: Day } deriving Typeable

instance VCacheable VDay where
  put = put . toModifiedJulianDay . unVDay
  get = VDay . ModifiedJulianDay <$> get

newtype VDiffTime = VDiffTime { unVDiffTime :: DiffTime } deriving Typeable

instance VCacheable VDiffTime where
  put = put . VRational . toRational . unVDiffTime
  get = VDiffTime . fromRational . unVRational <$> get

newtype VRational = VRational { unVRational :: Rational } deriving Typeable

instance VCacheable VRational where
  put (VRational x) = do
    put $ numerator   x
    put $ denominator x

  get = VRational <$> ((%) <$> get <*> get)

newtype VVector a = VVector { unVVector :: Vector a } deriving Typeable

instance VCacheable a => VCacheable (VVector a) where
  put (VVector x) = putVarNat (fromIntegral $ V.length x) >> V.forM_ x put
  get = getVarNat >>= \len -> VVector <$> V.replicateM (fromIntegral len) get

newtype VVersion = VVersion { unVVersion :: Version } deriving Typeable

instance VCacheable VVersion where
  put = put . versionBranch . unVVersion
  get = VVersion . flip Version [] <$> get
