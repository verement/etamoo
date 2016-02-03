
{-# LANGUAGE DeriveDataTypeable #-}

-- | Abstract MOO list type
module MOO.List (
    MOOList

  -- * Accessors
  -- ** Length information
  , length
  , null

  -- ** Indexing
  , (!)
  , head

  -- ** Extracting sublists (slicing)
  , slice
  , tail
  , splitAt

  -- * Construction
  -- ** Initialization
  , empty

  -- ** Concatenation
  , snoc
  , concat

  -- * Elementwise operations
  -- ** Monadic mapping
  , forM_

  -- * Working with predicates
  -- ** Searching
  , elem
  , findIndex
  , elemIndex

  -- * Folding
  -- ** Monadic folds
  , foldM

  -- * Conversions
  -- ** Lists
  , toList
  , fromList

  -- * MOO primitives
  , storageBytes
  , equal

  -- * Association list interface
  , assocLens

  -- * Convenience functions
  , set
  , insert
  , delete
  ) where

import Control.Applicative ((<$>))
import Data.Function (on)
import Data.HashMap.Lazy (HashMap)
import Data.Monoid (Monoid(mempty, mappend, mconcat), (<>))
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Database.VCache (VCacheable(put, get))
import Prelude hiding (concat, head, length, null, tail, elem, splitAt, (++))

import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import MOO.Types (Value(Lst, Str), StrT)
import MOO.Util (VVector(..))

import qualified MOO.Types as Value

type AssocMap = HashMap StrT (Int, Value)

data MOOList = MOOList {
    toVector   :: Vector Value
  , toAssocMap :: Maybe AssocMap
  } deriving Typeable

instance Eq MOOList where
  (==) = (==) `on` toVector

instance Monoid MOOList where
  mempty  = empty
  mappend = (++)
  mconcat = concat

instance Show MOOList where
  show = show . toList

instance VCacheable MOOList where
  put = put . VVector . toVector
  get = fromVector . unVVector <$> get

fromVector :: Vector Value -> MOOList
fromVector vec = MOOList {
    toVector   = vec
  , toAssocMap = vectorToAssocMap vec
  }

fromList :: [Value] -> MOOList
fromList = fromVector . V.fromList

toList :: MOOList -> [Value]
toList = V.toList . toVector

storageBytes :: MOOList -> Int
storageBytes = V.sum . V.map Value.storageBytes . toVector

equal :: MOOList -> MOOList -> Bool
equal = vectorEqual `on` toVector

  where x `vectorEqual` y = V.length x == V.length y &&
                            V.and (V.zipWith Value.equal x y)

empty :: MOOList
empty = fromVector V.empty

(!) :: MOOList -> Int -> Value
lst ! i = toVector lst V.! i

head :: MOOList -> Value
head = V.head . toVector

tail :: MOOList -> MOOList
tail = fromVector . V.tail . toVector

slice :: Int -> Int -> MOOList -> MOOList
slice i n = fromVector . V.slice i n . toVector

splitAt :: Int -> MOOList -> (MOOList, MOOList)
splitAt n lst = let (b, a) = V.splitAt n (toVector lst)
                in (fromVector b, fromVector a)

snoc :: MOOList -> Value -> MOOList
snoc lst = fromVector . V.snoc (toVector lst)

(++) :: MOOList -> MOOList -> MOOList
x ++ y = fromVector $ toVector x <> toVector y

concat :: [MOOList] -> MOOList
concat = fromVector . V.concat . map toVector

length :: MOOList -> Int
length = V.length . toVector

null :: MOOList -> Bool
null = V.null . toVector

elem :: Value -> MOOList -> Bool
elem x = V.elem x . toVector

elemIndex :: Value -> MOOList -> Maybe Int
elemIndex x = V.elemIndex x . toVector

findIndex :: (Value -> Bool) -> MOOList -> Maybe Int
findIndex p = V.findIndex p . toVector

foldM :: Monad m => (a -> Value -> m a) -> a -> MOOList -> m a
foldM f acc = V.foldM f acc . toVector

forM_ :: Monad m => MOOList -> (Value -> m b) -> m ()
forM_ = V.forM_ . toVector

-- Association list interface

vectorToAssocMap :: Vector Value -> Maybe AssocMap
vectorToAssocMap = fmap snd . V.foldM mkAssocMap (0, HM.empty)

  where mkAssocMap :: (Int, AssocMap) -> Value -> Maybe (Int, AssocMap)
        mkAssocMap (i, map) (Lst lst) = case toList lst of
          [Str k, value] -> let map' = assocMapInsert k (i, value) map
                            in Just (succ $! i, map')
          [_    , _    ] ->    Just (succ $! i, map)
          _              -> Nothing
        mkAssocMap _ _ = Nothing

        -- Preserve the first value associated with duplicate keys
        assocMapInsert :: StrT -> (Int, Value) -> AssocMap -> AssocMap
        assocMapInsert = HM.insertWith (flip const)

-- | Return the current value (if any) associated with the given key, and a
-- function to associate the key with a new value (or remove it). Returns
-- 'Nothing' if the list is not a proper association list.
assocLens :: StrT -> MOOList -> Maybe (Maybe Value, Maybe Value -> MOOList)
assocLens key lst = mkLens <$> toAssocMap lst

  where mkLens :: AssocMap -> (Maybe Value, Maybe Value -> MOOList)
        mkLens map = (snd <$> current, setValue)

          where current :: Maybe (Int, Value)
                current = HM.lookup key map

                setValue :: Maybe Value -> MOOList
                setValue (Just newValue) =
                  let vec    = toVector lst
                      assoc  = Lst $ fromList [Str key, newValue]
                      map' i = HM.insert key (i, newValue) map
                  in case current of
                       Nothing -> lst {
                           toVector   = V.snoc vec assoc
                         , toAssocMap = Just $ map' (V.length vec)
                         }
                       Just (i, _) -> lst {
                           toVector   = vectorSet vec assoc i
                         , toAssocMap = Just $ map' i
                         }
                setValue Nothing = maybe lst (delete lst . fst) current

-- Convenience functions

-- | Return a modified list with the given 0-based index replaced with the
-- given value.
set :: MOOList -> Value -> Int -> MOOList
set lst value = fromVector . vectorSet (toVector lst) value

vectorSet :: Vector Value -> Value -> Int -> Vector Value
vectorSet vec value i = V.modify (\vec' -> VM.write vec' i value) vec

-- | Return a modified list with the given value inserted at the given 0-based
-- index.
insert :: MOOList -> Int -> Value -> MOOList
insert lst i = fromVector . vectorInsert (toVector lst) i

  where vectorInsert :: Vector Value -> Int -> Value -> Vector Value
        vectorInsert vec index value
          | index <= 0      = V.cons value vec
          | index >= vecLen = V.snoc vec value
          | otherwise       = V.create $ do
              vec' <- flip VM.grow 1 =<< V.thaw vec
              let moveLen = vecLen - index
                  s = VM.slice  index      moveLen vec'
                  t = VM.slice (index + 1) moveLen vec'
              VM.move t s
              VM.write vec' index value
              return vec'
          where vecLen = V.length vec

-- | Return a modified list with the value at the given 0-based index removed.
delete :: MOOList -> Int -> MOOList
delete lst i = fromVector $ vectorDelete (toVector lst) i

  where vectorDelete :: Vector Value -> Int -> Vector Value
        vectorDelete vec index
          | index == 0          = V.tail vec
          | index == vecLen - 1 = V.init vec
          | index * 2 < vecLen  = V.tail $ (`V.modify` vec) $ \vec' ->
            let s = VM.slice 0 index vec'
                t = VM.slice 1 index vec'
            in VM.move t s
          | otherwise            = V.init $ (`V.modify` vec) $ \vec' ->
            let moveLen = vecLen - index - 1
                s = VM.slice (index + 1) moveLen vec'
                t = VM.slice  index      moveLen vec'
            in VM.move t s
          where vecLen = V.length vec
