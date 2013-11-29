
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
                 , error2text
                 , text2binary
                 ) where

import           Data.Int
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Vector as V
import           Data.Vector (Vector)
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)

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

truthOf :: Value -> Bool
truthOf (Int x)  = case x of 0   -> False; _ -> True
truthOf (Flt x)  = case x of 0.0 -> False; _ -> True
truthOf (Str x)  = case x of ""  -> False; _ -> True
truthOf (Lst vs) = not (V.null vs)
truthOf _        = False

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

text2binary :: Text -> Maybe ByteString
text2binary t = do
  w8s <- translate $ T.unpack t
  return $ BS.pack w8s
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
          | x >= '0' && x <= '9' = Just $      distance '0' x
          | x >= 'a' && x <= 'f' = Just $ 10 + distance 'a' x
          | x >= 'A' && x <= 'F' = Just $ 10 + distance 'A' x
          | otherwise            = Nothing
          where distance c0 c1 = fromIntegral $ fromEnum c1 - fromEnum c0
