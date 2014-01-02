
{-# LANGUAGE OverloadedStrings #-}

module MOO.Types ( IntT
                 , FltT
                 , StrT
                 , ObjT
                 , ErrT
                 , LstT
                 , ObjId
                 , Id
                 , Type(..)
                 , Value(..)
                 , Error(..)
                 , Sizeable(..)
                 , nothing
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
                 , validStrChar
                 , listSet
                 ) where

import Data.Int
import Data.Word
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Char (isAscii, isPrint, isDigit)
import Foreign.Storable (sizeOf)

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

class Sizeable t where
  storageBytes :: t -> Int

instance Sizeable () where
  storageBytes _ = sizeOf (0 :: Int)

instance Sizeable Bool where
  storageBytes = sizeOf

instance Sizeable Int where
  storageBytes = sizeOf

instance Sizeable Int32 where
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

type IntT = Int32
type FltT = Double
type StrT = Text
type ObjT = Int
type ErrT = Error
type LstT = Vector Value

type ObjId = ObjT
type Id    = StrT

data Value = Int !IntT
           | Flt !FltT
           | Str !StrT
           | Obj !ObjT
           | Err !ErrT
           | Lst !LstT
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

-- Case-sensitive equality
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

data Type = TAny
          | TNum
          | TInt
          | TFlt
          | TStr
          | TObj
          | TErr
          | TLst
          deriving (Eq, Show)

data Error = E_NONE
           | E_TYPE
           | E_DIV
           | E_PERM
           | E_PROPNF
           | E_VERBNF
           | E_VARNF
           | E_INVIND
           | E_RECMOVE
           | E_MAXREC
           | E_RANGE
           | E_ARGS
           | E_NACC
           | E_INVARG
           | E_QUOTA
           | E_FLOAT
           deriving (Eq, Ord, Enum, Bounded, Show)

instance Sizeable Error where
  storageBytes _ = storageBytes ()

truthOf :: Value -> Bool
truthOf (Int x) = x /= 0
truthOf (Flt x) = x /= 0.0
truthOf (Str t) = not (T.null t)
truthOf (Lst v) = not (V.null v)
truthOf _       = False

truthValue :: Bool -> Value
truthValue False = Int 0
truthValue True  = Int 1

typeOf :: Value -> Type
typeOf Int{} = TInt
typeOf Flt{} = TFlt
typeOf Str{} = TStr
typeOf Obj{} = TObj
typeOf Err{} = TErr
typeOf Lst{} = TLst

typeCode :: Type -> IntT
typeCode TNum = -2
typeCode TAny = -1
typeCode TInt =  0
typeCode TObj =  1
typeCode TStr =  2
typeCode TErr =  3
typeCode TLst =  4
typeCode TFlt =  9

toText :: Value -> Text
toText (Int x) = T.pack $ show x
toText (Flt x) = T.pack $ show x
toText (Str x) = x
toText (Obj x) = T.pack $ '#' : show x
toText (Err x) = error2text x
toText (Lst _) = "{list}"

toLiteral :: Value -> Text
toLiteral (Lst vs) = T.concat
                     ["{"
                     , T.intercalate ", " $ map toLiteral (V.toList vs)
                     , "}"]
toLiteral (Str x) = T.concat ["\"", T.concatMap escape x, "\""]
  where escape '"'  = "\\\""
        escape '\\' = "\\\\"
        escape c    = T.singleton c
toLiteral (Err x) = T.pack $ show x
toLiteral v = toText v

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

validStrChar :: Char -> Bool
validStrChar c = isAscii c && (isPrint c || c == '\t')

listSet :: LstT -> Int -> Value -> LstT
listSet v i value = V.modify (\m -> VM.write m (i - 1) value) v
