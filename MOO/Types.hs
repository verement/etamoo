
{-# LANGUAGE OverloadedStrings #-}

module MOO.Types ( IntT
                 , FltT
                 , StrT
                 , ObjT
                 , ErrT
                 , ObjId
                 , Id
                 , Type(..)
                 , Value(..)
                 , Error(..)
                 , truthOf
                 , truthValue
                 , typeCode
                 , toText
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

type ObjId = ObjT
type Id    = StrT

data Value = Int !IntT
           | Flt !FltT
           | Str !StrT
           | Obj !ObjT
           | Err !ErrT
           | Lst !(Vector Value)
           deriving (Eq, Show)

data Type = TAny
          | TInt
          | TFlt
          | TNum
          | TStr
          | TObj
          | TErr
          | TLst

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
           deriving (Eq, Enum, Bounded, Show)

truthOf :: Value -> Bool
truthOf (Int x)  = case x of 0   -> False; _ -> True
truthOf (Flt x)  = case x of 0.0 -> False; _ -> True
truthOf (Str x)  = case x of ""  -> False; _ -> True
truthOf (Lst vs) = not (V.null vs)
truthOf _        = False

truthValue :: Bool -> Value
truthValue False = Int 0
truthValue True  = Int 1

typeCode :: Value -> IntT
typeCode (Int _) = 0
typeCode (Obj _) = 1
typeCode (Str _) = 2
typeCode (Err _) = 3
typeCode (Lst _) = 4
typeCode (Flt _) = 9

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
