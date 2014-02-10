
-- | Abstract MOO string type
module MOO.String (
    MOOString

  -- * Creation and elimination
  , fromText
  , toText
  , toCaseFold
  , fromString
  , toString
  , toRegexp
  , singleton
  , empty

  -- * Basic interface
  , append
  , tail
  , null
  , length
  , compareLength
  , storageBytes
  , equal

  -- * Transformations
  , intercalate

  -- * Special folds
  , concat
  , concatMap

  -- * Substrings
  -- ** Breaking strings
  , take
  , drop
  , splitAt
  , breakOn
  , breakOnEnd
  -- ** Breaking into many substrings
  , splitOn
  -- ** Breaking into lines and words
  , words
  , unwords

  -- * Indexing
  , index
  ) where

import Data.Function (on)
import Data.Hashable (Hashable(..))
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Data.Text (Text)
import Foreign.Storable (sizeOf)
import Prelude hiding (tail, null, length, concat, concatMap, take, drop,
                       splitAt, words, unwords)

import qualified Data.Text as T

import MOO.Builtins.Match (Regexp, newRegexp)

type CompiledRegexp = Either (String, Int) Regexp

data MOOString = MOOString {
    toText         :: Text
  , toCaseFold     :: Text
  , length         :: Int
  , regexp         :: CompiledRegexp
  , regexpCaseless :: CompiledRegexp
  }

instance IsString MOOString where
  fromString = fromText . T.pack

instance Eq MOOString where
  (==) = (==) `on` toCaseFold

instance Ord MOOString where
  compare = compare `on` toCaseFold

instance Hashable MOOString where
  hashWithSalt salt = hashWithSalt salt . toCaseFold

instance Monoid MOOString where
  mempty  = empty
  mappend = append
  mconcat = concat

instance Show MOOString where
  show = show . toText

fromText :: Text -> MOOString
fromText text = MOOString {
    toText         = text
  , toCaseFold     = caseFold text
  , length         = T.length text
  , regexp         = newRegexp text True
  , regexpCaseless = newRegexp text False
  }

toString :: MOOString -> String
toString = T.unpack . toText

toRegexp :: Bool  -- ^ case-matters
         -> MOOString -> CompiledRegexp
toRegexp True  = regexp
toRegexp False = regexpCaseless

-- | Case-fold the argument, returning the same argument if the result is
-- unchanged to avoid wasting memory.
caseFold :: Text -> Text
caseFold text
  | text == folded = text
  | otherwise      = folded
  where folded = T.toCaseFold text

singleton :: Char -> MOOString
singleton = fromText . T.singleton

storageBytes :: MOOString -> Int
storageBytes str = sizeOf 'x' * (length str + 1) +
                   sizeOf (undefined :: Int) * 4

-- | Test two strings for indistinguishable (case-sensitive) equality.
equal :: MOOString -> MOOString -> Bool
equal = (==) `on` toText

empty :: MOOString
empty = fromText T.empty

tail :: MOOString -> MOOString
tail = fromText . T.tail . toText

append :: MOOString -> MOOString -> MOOString
append str1 str2 = fromText $ T.append (toText str1) (toText str2)

null :: MOOString -> Bool
null = T.null . toText

compareLength :: MOOString -> Int -> Ordering
compareLength str = T.compareLength (toText str)

intercalate :: MOOString -> [MOOString] -> MOOString
intercalate sep = fromText . T.intercalate (toText sep) . map toText

concat :: [MOOString] -> MOOString
concat = fromText . T.concat . map toText

concatMap :: (Char -> MOOString) -> MOOString -> MOOString
concatMap f = fromText . T.concatMap (toText . f) . toText

take :: Int -> MOOString -> MOOString
take len = fromText . T.take len . toText

drop :: Int -> MOOString -> MOOString
drop len = fromText . T.drop len . toText

splitAt :: Int -> MOOString -> (MOOString, MOOString)
splitAt n str = (fromText prefix, fromText remainder)
  where (prefix, remainder) = T.splitAt n (toText str)

-- XXX Need caseless versions...

breakOn :: MOOString -> MOOString -> (MOOString, MOOString)
breakOn sep str = (fromText before, fromText match)
  where (before, match) = T.breakOn (toText sep) (toText str)

breakOnEnd :: MOOString -> MOOString -> (MOOString, MOOString)
breakOnEnd sep str = (fromText before, fromText match)
  where (before, match) = T.breakOnEnd (toText sep) (toText str)

splitOn :: MOOString -> MOOString -> [MOOString]
splitOn sep = map fromText . T.splitOn (toText sep) . toText

--

words :: MOOString -> [MOOString]
words = map fromText . T.words . toText

unwords :: [MOOString] -> MOOString
unwords = fromText . T.unwords . map toText

index :: MOOString -> Int -> Char
index str =  T.index (toText str)
