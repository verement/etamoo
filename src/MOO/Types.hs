
{-# LANGUAGE OverloadedStrings #-}

-- | Basic data types used throughout the MOO server code
module MOO.Types (

  -- * Haskell Types Representing MOO Values
    IntT
  , FltT
  , StrT
  , ObjT
  , ErrT
  , LstT

  , ObjId
  , Id

  -- * MOO Type and Value Reification
  , Type(..)
  , Value(..)
  , Error(..)

  , nothing

  -- * Type and Value Functions
  , fromInt
  , fromFlt
  , fromStr
  , fromObj
  , fromErr
  , fromLst

  , equal
  , truthOf
  , truthValue
  , typeOf

  , typeCode

  , toText
  , toLiteral
  , error2text

  , text2binary

  -- * List Convenience Functions
  , fromList
  , fromListBy
  , stringList
  , objectList

  , listSet

  -- * Miscellaneous
  , validStrChar
  , endOfTime

  -- * Estimating Haskell Storage Sizes
  , Sizeable(..)

  ) where

import Control.Concurrent (ThreadId)
import Data.Char (isAscii, isPrint, isDigit)
import Data.Int (Int32, Int64)
import Data.Map (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Vector (Vector)
import Data.Word (Word8)
import Foreign.Storable (sizeOf)
import System.Random (StdGen)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- | The 'Sizeable' class is used to estimate the storage requirements of
-- various values, for use by several built-in functions which are supposed to
-- return the byte sizes of certain internal structures.
--
-- The sizes calculated by instances of this class are necessarily
-- approximate, since it may be difficult or impossible to measure them
-- precisely in the Haskell runtime environment.
class Sizeable t where
  -- | Return the estimated storage size of the given value, in bytes.
  storageBytes :: t -> Int

instance Sizeable () where
  storageBytes _ = sizeOf (undefined :: Int)

instance Sizeable Bool where
  storageBytes = sizeOf

instance Sizeable Int where
  storageBytes = sizeOf

instance Sizeable Int32 where
  storageBytes = sizeOf

instance Sizeable Int64 where
  storageBytes = sizeOf

instance Sizeable Double where
  storageBytes = sizeOf

instance Sizeable Text where
  storageBytes t = sizeOf 'x' * (T.length t + 1)

instance Sizeable a => Sizeable (Vector a) where
  storageBytes v = V.sum (V.map storageBytes v)

instance Sizeable a => Sizeable [a] where
  storageBytes = foldr bytes (storageBytes ())
    where bytes x s = s + storageBytes () + storageBytes x

instance Sizeable a => Sizeable (Maybe a) where
  storageBytes Nothing  = storageBytes ()
  storageBytes (Just x) = storageBytes () + storageBytes x

instance (Sizeable a, Sizeable b) => Sizeable (a, b) where
  storageBytes (x, y) = storageBytes () + storageBytes x + storageBytes y

instance (Sizeable k, Sizeable v) => Sizeable (Map k v) where
  storageBytes =
    M.foldrWithKey' (\k v s -> s + storageBytes k + storageBytes v) 0

instance Sizeable StdGen where
  storageBytes _ = storageBytes () + 2 * storageBytes (undefined :: Int32)

instance Sizeable UTCTime where
  storageBytes _ = 4 * storageBytes ()

instance Sizeable ThreadId where
  storageBytes _ = storageBytes ()

type IntT = Int32
                          -- ^ MOO integer
type FltT = Double        -- ^ MOO floating-point number
type StrT = Text          -- ^ MOO string
type ObjT = Int           -- ^ MOO object number
type ErrT = Error         -- ^ MOO error
type LstT = Vector Value  -- ^ MOO list

type ObjId = ObjT         -- ^ MOO object number
type Id    = StrT         -- ^ MOO identifier (string)

-- | A 'Value' represents any MOO value.
data Value = Int !IntT  -- ^ integer
           | Flt !FltT  -- ^ floating-point number
           | Str !StrT  -- ^ string
           | Obj !ObjT  -- ^ object number
           | Err !ErrT  -- ^ error
           | Lst !LstT  -- ^ list
           deriving Show

instance Sizeable Value where
  storageBytes value = case value of
    Int x -> box + storageBytes x
    Flt x -> box + storageBytes x
    Str x -> box + storageBytes x
    Obj x -> box + storageBytes x
    Err x -> box + storageBytes x
    Lst x -> box + storageBytes x
    where box = storageBytes ()

-- | The default false value (zero)
nothing :: Value
nothing = truthValue False

fromInt :: Value -> IntT
fromInt (Int x) = x

fromFlt :: Value -> FltT
fromFlt (Flt x) = x

fromStr :: Value -> StrT
fromStr (Str x) = x

fromObj :: Value -> ObjT
fromObj (Obj x) = x

fromErr :: Value -> ErrT
fromErr (Err x) = x

fromLst :: Value -> LstT
fromLst (Lst x) = x

-- Case-insensitive equality
instance Eq Value where
  (Int a) == (Int b) = a == b
  (Flt a) == (Flt b) = a == b
  (Str a) == (Str b) = T.toCaseFold a == T.toCaseFold b
  (Obj a) == (Obj b) = a == b
  (Err a) == (Err b) = a == b
  (Lst a) == (Lst b) = a == b
  _       == _       = False

-- | Test two MOO values for indistinguishable (case-sensitive) equality.
equal :: Value -> Value -> Bool
(Str a) `equal` (Str b) = a == b
(Lst a) `equal` (Lst b) = V.length a == V.length b &&
                          V.and (V.zipWith equal a b)
x       `equal` y       = x == y

-- Case-insensitive ordering
instance Ord Value where
  (Int a) `compare` (Int b) = a `compare` b
  (Flt a) `compare` (Flt b) = a `compare` b
  (Str a) `compare` (Str b) = T.toCaseFold a `compare` T.toCaseFold b
  (Obj a) `compare` (Obj b) = a `compare` b
  (Err a) `compare` (Err b) = a `compare` b
  _       `compare` _       = error "Illegal comparison"

-- | A 'Type' represents one or more MOO value types.
data Type = TAny  -- ^ any type
          | TNum  -- ^ integer or floating-point number
          | TInt  -- ^ integer
          | TFlt  -- ^ floating-point number
          | TStr  -- ^ string
          | TObj  -- ^ object number
          | TErr  -- ^ error
          | TLst  -- ^ list
          deriving (Eq, Show)

-- | A MOO error
data Error = E_NONE     -- ^ No error
           | E_TYPE     -- ^ Type mismatch
           | E_DIV      -- ^ Division by zero
           | E_PERM     -- ^ Permission denied
           | E_PROPNF   -- ^ Property not found
           | E_VERBNF   -- ^ Verb not found
           | E_VARNF    -- ^ Variable not found
           | E_INVIND   -- ^ Invalid indirection
           | E_RECMOVE  -- ^ Recursive move
           | E_MAXREC   -- ^ Too many verb calls
           | E_RANGE    -- ^ Range error
           | E_ARGS     -- ^ Incorrect number of arguments
           | E_NACC     -- ^ Move refused by destination
           | E_INVARG   -- ^ Invalid argument
           | E_QUOTA    -- ^ Resource limit exceeded
           | E_FLOAT    -- ^ Floating-point arithmetic error
           deriving (Eq, Ord, Enum, Bounded, Show)

instance Sizeable Error where
  storageBytes _ = storageBytes ()

-- | Is the given MOO value considered to be /true/ or /false/?
truthOf :: Value -> Bool
truthOf (Int x) = x /= 0
truthOf (Flt x) = x /= 0.0
truthOf (Str t) = not (T.null t)
truthOf (Lst v) = not (V.null v)
truthOf _       = False

-- | Return a default MOO value (integer) having the given boolean value.
truthValue :: Bool -> Value
truthValue False = Int 0
truthValue True  = Int 1

-- | Return a 'Type' indicating the type of the given value.
typeOf :: Value -> Type
typeOf Int{} = TInt
typeOf Flt{} = TFlt
typeOf Str{} = TStr
typeOf Obj{} = TObj
typeOf Err{} = TErr
typeOf Lst{} = TLst

-- | Return an integer code corresponding to the given type. These codes are
-- visible to MOO code via the @typeof()@ built-in function and various
-- predefined variables.
typeCode :: Type -> IntT
typeCode TNum = -2
typeCode TAny = -1
typeCode TInt =  0
typeCode TObj =  1
typeCode TStr =  2
typeCode TErr =  3
typeCode TLst =  4
typeCode TFlt =  9

-- | Return a string representation of the given MOO value, using the same
-- rules as the @tostr()@ built-in function.
toText :: Value -> Text
toText (Int x) = T.pack $ show x
toText (Flt x) = T.pack $ show x
toText (Str x) = x
toText (Obj x) = T.pack $ '#' : show x
toText (Err x) = error2text x
toText (Lst _) = "{list}"

-- | Return a literal representation of the given MOO value, using the same
-- rules as the @toliteral()@ built-in function.
toLiteral :: Value -> Text
toLiteral (Lst vs) = T.concat
                     [ "{"
                     , T.intercalate ", " $ map toLiteral (V.toList vs)
                     , "}"]
toLiteral (Str x) = T.concat ["\"", T.concatMap escape x, "\""]
  where escape '"'  = "\\\""
        escape '\\' = "\\\\"
        escape c    = T.singleton c
toLiteral (Err x) = T.pack $ show x
toLiteral v = toText v

-- | Return a string description of the given error value.
error2text :: Error -> Text
error2text E_NONE    = "No error"
error2text E_TYPE    = "Type mismatch"
error2text E_DIV     = "Division by zero"
error2text E_PERM    = "Permission denied"
error2text E_PROPNF  = "Property not found"
error2text E_VERBNF  = "Verb not found"
error2text E_VARNF   = "Variable not found"
error2text E_INVIND  = "Invalid indirection"
error2text E_RECMOVE = "Recursive move"
error2text E_MAXREC  = "Too many verb calls"
error2text E_RANGE   = "Range error"
error2text E_ARGS    = "Incorrect number of arguments"
error2text E_NACC    = "Move refused by destination"
error2text E_INVARG  = "Invalid argument"
error2text E_QUOTA   = "Resource limit exceeded"
error2text E_FLOAT   = "Floating-point arithmetic error"

-- | Parse a MOO /binary string/ and return the corresponding byte values, or
-- 'Nothing' if the string is improperly formatted.
text2binary :: Text -> Maybe [Word8]
text2binary = translate . T.unpack
  where translate ('~':x:y:rs) = do
          xv <- hexValue x
          yv <- hexValue y
          let b = 16 * xv + yv
          bs <- translate rs
          return (b:bs)
        translate ('~':_) = Nothing
        translate (c:cs) = do
          let b = fromIntegral $ fromEnum c
          bs <- translate cs
          return (b:bs)
        translate [] = return []
        hexValue x
          | isDigit x            = Just $      distance '0' x
          | x >= 'a' && x <= 'f' = Just $ 10 + distance 'a' x
          | x >= 'A' && x <= 'F' = Just $ 10 + distance 'A' x
          | otherwise            = Nothing
          where distance c0 c1 = fromIntegral $ fromEnum c1 - fromEnum c0

-- | May the given character appear in a valid MOO string?
validStrChar :: Char -> Bool
validStrChar c = isAscii c && (isPrint c || c == '\t')

-- | Turn a Haskell list into a MOO list.
fromList :: [Value] -> Value
fromList = Lst . V.fromList

-- | Turn a Haskell list into a MOO list, using a function to map Haskell
-- values to MOO values.
fromListBy :: (a -> Value) -> [a] -> Value
fromListBy f = fromList . map f

-- | Turn a list of strings into a MOO list.
stringList :: [StrT] -> Value
stringList = fromListBy Str

-- | Turn a list of object numbers into a MOO list.
objectList :: [ObjT] -> Value
objectList = fromListBy Obj

-- | Return a modified list with the given 1-based index replaced with the
-- given value.
listSet :: LstT -> Int -> Value -> LstT
listSet v i value = V.modify (\m -> VM.write m (i - 1) value) v

-- | This is the last UTC time value representable as a signed 32-bit
-- seconds-since-1970 value. Unfortunately it is used as a sentinel value in
-- LambdaMOO to represent the starting time of indefinitely suspended tasks,
-- so we really can't support time values beyond this point... yet.
endOfTime :: UTCTime
endOfTime = posixSecondsToUTCTime $ fromIntegral (maxBound :: Int32)
