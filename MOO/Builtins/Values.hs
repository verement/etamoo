
{-# LANGUAGE OverloadedStrings #-}

module MOO.Builtins.Values ( builtins ) where

import Control.Monad (mplus)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (bracket)
import Control.Concurrent.MVar (newMVar, takeMVar, putMVar)
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)
import Foreign.Storable (sizeOf)
import Foreign.C (CString, withCString, peekCString)
import Data.Maybe (fromJust)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Data.Digest.Pure.MD5 (MD5Digest)
import qualified Data.Digest.Pure.MD5 as MD5

import MOO.Types
import MOO.Execution
import MOO.Parser (parseNum, parseObj)
import MOO.Builtins.Common

-- 4.4.2 Manipulating MOO Values

builtins :: [BuiltinSpec]
builtins = [
    ("typeof"        , (bf_typeof        , Info 1 (Just 1) [TAny]       TInt))
  , ("tostr"         , (bf_tostr         , Info 0 Nothing  []           TStr))
  , ("toliteral"     , (bf_toliteral     , Info 1 (Just 1) [TAny]       TStr))
  , ("toint"         , (bf_toint         , Info 1 (Just 1) [TAny]       TInt))
  , ("tonum"         , fromJust $ lookup "toint" builtins)
  , ("toobj"         , (bf_toobj         , Info 1 (Just 1) [TAny]       TObj))
  , ("tofloat"       , (bf_tofloat       , Info 1 (Just 1) [TAny]       TFlt))
  , ("equal"         , (bf_equal         , Info 2 (Just 2) [TAny, TAny] TInt))
  , ("value_bytes"   , (bf_value_bytes   , Info 1 (Just 1) [TAny]       TInt))
  , ("value_hash"    , (bf_value_hash    , Info 1 (Just 1) [TAny]       TStr))

  , ("random"        , (bf_random        , Info 0 (Just 1) [TInt]       TInt))
  , ("min"           , (bf_min           , Info 1 Nothing  [TNum]       TNum))
  , ("max"           , (bf_max           , Info 1 Nothing  [TNum]       TNum))
  , ("abs"           , (bf_abs           , Info 1 (Just 1) [TNum]       TNum))
  , ("floatstr"      , (bf_floatstr      , Info 2 (Just 3) [TFlt, TInt,
                                                            TAny]       TStr))
  , ("sqrt"          , (bf_sqrt          , Info 1 (Just 1) [TFlt]       TFlt))
  , ("sin"           , (bf_sin           , Info 1 (Just 1) [TFlt]       TFlt))
  , ("cos"           , (bf_cos           , Info 1 (Just 1) [TFlt]       TFlt))
  , ("tan"           , (bf_tan           , Info 1 (Just 1) [TFlt]       TFlt))
  , ("asin"          , (bf_asin          , Info 1 (Just 1) [TFlt]       TFlt))
  , ("acos"          , (bf_acos          , Info 1 (Just 1) [TFlt]       TFlt))
  , ("atan"          , (bf_atan          , Info 1 (Just 2) [TFlt, TFlt] TFlt))
  , ("sinh"          , (bf_sinh          , Info 1 (Just 1) [TFlt]       TFlt))
  , ("cosh"          , (bf_cosh          , Info 1 (Just 1) [TFlt]       TFlt))
  , ("tanh"          , (bf_tanh          , Info 1 (Just 1) [TFlt]       TFlt))
  , ("exp"           , (bf_exp           , Info 1 (Just 1) [TFlt]       TFlt))
  , ("log"           , (bf_log           , Info 1 (Just 1) [TFlt]       TFlt))
  , ("log10"         , (bf_log10         , Info 1 (Just 1) [TFlt]       TFlt))
  , ("ceil"          , (bf_ceil          , Info 1 (Just 1) [TFlt]       TFlt))
  , ("floor"         , (bf_floor         , Info 1 (Just 1) [TFlt]       TFlt))
  , ("trunc"         , (bf_trunc         , Info 1 (Just 1) [TFlt]       TFlt))

  , ("length"        , (bf_length        , Info 1 (Just 1) [TAny]       TInt))
  , ("strsub"        , (bf_strsub        , Info 3 (Just 4) [TStr, TStr,
                                                            TStr, TAny] TStr))
  , ("index"         , (bf_index         , Info 2 (Just 3) [TStr, TStr,
                                                            TAny]       TInt))
  , ("rindex"        , (bf_rindex        , Info 2 (Just 3) [TStr, TStr,
                                                            TAny]       TInt))
  , ("strcmp"        , (bf_strcmp        , Info 2 (Just 2) [TStr, TStr] TInt))
  , ("decode_binary" , (bf_decode_binary , Info 1 (Just 2) [TStr, TAny] TLst))
  , ("encode_binary" , (bf_encode_binary , Info 0 Nothing  []           TStr))
  , ("match"         , (bf_match         , Info 2 (Just 3) [TStr, TStr,
                                                            TAny]       TLst))
  , ("rmatch"        , (bf_rmatch        , Info 2 (Just 3) [TStr, TStr,
                                                            TAny]       TLst))
  , ("substitute"    , (bf_substitute    , Info 2 (Just 2) [TStr, TLst] TStr))
  , ("crypt"         , (bf_crypt         , Info 1 (Just 2) [TStr, TStr] TStr))
  , ("string_hash"   , (bf_string_hash   , Info 1 (Just 1) [TStr]       TStr))
  , ("binary_hash"   , (bf_binary_hash   , Info 1 (Just 1) [TStr]       TStr))

  , ("is_member"     , (bf_is_member     , Info 2 (Just 2) [TAny, TLst] TInt))
  , ("listinsert"    , (bf_listinsert    , Info 2 (Just 3) [TLst, TAny,
                                                            TInt]       TLst))
  , ("listappend"    , (bf_listappend    , Info 2 (Just 3) [TLst, TAny,
                                                            TInt]       TLst))
  , ("listdelete"    , (bf_listdelete    , Info 2 (Just 2) [TLst, TInt] TLst))
  , ("listset"       , (bf_listset       , Info 3 (Just 3) [TLst, TAny,
                                                            TInt]       TLst))
  , ("setadd"        , (bf_setadd        , Info 2 (Just 2) [TLst, TAny] TLst))
  , ("setremove"     , (bf_setremove     , Info 2 (Just 2) [TLst, TAny] TLst))
  ]

-- 4.4.2.1 General Operations Applicable to all Values

bf_typeof [value] = return $ Int $ typeCode $ typeOf value

bf_tostr values = return $ Str $ T.concat $ map toText values

bf_toliteral [value] = return $ Str $ toliteral value
  where toliteral (Lst vs) = T.concat
                             ["{"
                             , T.intercalate ", " $ map toliteral (V.toList vs)
                             , "}"]
        toliteral (Str x) = T.concat ["\"", T.concatMap escape x, "\""]
          where escape '"'  = "\\\""
                escape '\\' = "\\\\"
                escape c    = T.singleton c
        toliteral (Err x) = T.pack $ show x
        toliteral v = toText v

-- XXX toint(" - 34  ") does not parse as -34
bf_toint [value] = toint value
  where toint value = case value of
          Int _ -> return value
          Flt x | x >= 0    -> if x > fromIntegral (maxBound :: IntT)
                               then raise E_FLOAT else return (Int $ floor   x)
                | otherwise -> if x < fromIntegral (minBound :: IntT)
                               then raise E_FLOAT else return (Int $ ceiling x)
          Obj x -> return (Int $ fromIntegral x)
          Str x -> maybe (return $ Int 0) toint (parseNum x)
          Err x -> return (Int $ fromIntegral $ fromEnum x)
          Lst _ -> raise E_TYPE

bf_toobj [value] = toobj value
  where toobj value = case value of
          Int x -> return (Obj $ fromIntegral x)
          Flt x | x >= 0    -> if x > fromIntegral (maxBound :: ObjT)
                               then raise E_FLOAT else return (Obj $ floor   x)
                | otherwise -> if x < fromIntegral (minBound :: ObjT)
                               then raise E_FLOAT else return (Obj $ ceiling x)
          Obj _ -> return value
          Str x -> maybe (return $ Obj 0) toobj $ parseNum x `mplus` parseObj x
          Err x -> return (Obj $ fromIntegral $ fromEnum x)
          Lst _ -> raise E_TYPE

bf_tofloat [value] = tofloat value
  where tofloat value = case value of
          Int x -> return (Flt $ fromIntegral x)
          Flt _ -> return value
          Obj x -> return (Flt $ fromIntegral x)
          Str x -> maybe (return $ Flt 0) tofloat (parseNum x)
          Err x -> return (Flt $ fromIntegral $ fromEnum x)
          Lst _ -> raise E_TYPE

bf_equal [value1, value2] = return $ truthValue (value1 `equal` value2)

bf_value_bytes [value] = return $ Int $ fromIntegral $ value_bytes value
  where value_bytes value = case value of
          -- Make stuff up ...
          Int x -> box + sizeOf x
          Flt x -> box + sizeOf x
          Obj x -> box + sizeOf x
          Str t -> box + sizeOf 'x' * (T.length t + 1)
          Err x -> box + sizeOf (0 :: Int)
          Lst v -> box + V.sum (V.map value_bytes v)
        box = 8

bf_value_hash [value] = do
  literal <- bf_toliteral [value]
  bf_string_hash [literal]

-- 4.4.2.2 Operations on Numbers

bf_random optional
  | mod <= 0  = raise E_INVARG
  | otherwise = fmap Int $ liftIO $ randomRIO (1, mod)
  where [Int mod] = defaults optional [Int maxBound]

bf_min ((Int x):xs) = minMaxInt min x xs
bf_min ((Flt x):xs) = minMaxFlt min x xs

bf_max ((Int x):xs) = minMaxInt max x xs
bf_max ((Flt x):xs) = minMaxFlt max x xs

minMaxInt :: (IntT -> IntT -> IntT) -> IntT -> [Value] -> MOO Value
minMaxInt f = go
  where go x (Int y:rs) = go (f x y) rs
        go x []         = return $ Int x
        go _ _          = raise E_TYPE

minMaxFlt :: (FltT -> FltT -> FltT) -> FltT -> [Value] -> MOO Value
minMaxFlt f = go
  where go x (Flt y:rs) = go (f x y) rs
        go x []         = return $ Flt x
        go _ _          = raise E_TYPE

bf_abs [Int x] = return $ Int $ abs x
bf_abs [Flt x] = return $ Flt $ abs x

bf_floatstr (Flt x : Int precision : optional)
  | precision < 0 = raise E_INVARG
  | otherwise = return $ Str $ T.pack $ printf format x
  where prec = min precision 19
        [scientific] = booleanDefaults optional [False]
        format = printf "%%.%d%c" prec $ if scientific then 'e' else 'f'

bf_sqrt  [Flt x] = checkFloat $ sqrt x

bf_sin   [Flt x] = checkFloat $ sin x
bf_cos   [Flt x] = checkFloat $ cos x
bf_tan   [Flt x] = checkFloat $ tan x

bf_asin  [Flt x] = checkFloat $ asin x
bf_acos  [Flt x] = checkFloat $ acos x
bf_atan  [Flt y] = checkFloat $ atan y
bf_atan  [Flt y,
          Flt x] = checkFloat $ atan2 y x

bf_sinh  [Flt x] = checkFloat $ sinh x
bf_cosh  [Flt x] = checkFloat $ cosh x
bf_tanh  [Flt x] = checkFloat $ tanh x

bf_exp   [Flt x] = checkFloat $ exp x
bf_log   [Flt x] = checkFloat $ log x
bf_log10 [Flt x] = checkFloat $ logBase 10 x

bf_ceil  [Flt x] = checkFloat $ fromIntegral $ ceiling x
bf_floor [Flt x] = checkFloat $ fromIntegral $ floor   x
bf_trunc [Flt x] | x < 0     = checkFloat $ fromIntegral $ ceiling x
                 | otherwise = checkFloat $ fromIntegral $ floor   x

-- 4.4.2.3 Operations on Strings

bf_length [Str string] = return (Int $ fromIntegral $ T.length string)
bf_length [Lst list]   = return (Int $ fromIntegral $ V.length list)
bf_length _            = raise E_TYPE

bf_strsub (Str subject : Str what : Str with : optional) = notyet

bf_index  (Str str1 : Str str2 : optional) = notyet
bf_rindex (Str str1 : Str str2 : optional) = notyet

bf_strcmp [Str str1, Str str2] =
  return $ Int $ case compare str1 str2 of
    LT -> -1
    EQ ->  0
    GT ->  1

bf_decode_binary (Str bin_string : optional) = notyet
bf_encode_binary args = notyet

bf_match  (Str subject : Str pattern : optional) = notyet
bf_rmatch (Str subject : Str pattern : optional) = notyet

bf_substitute [Str template, Lst subs] = notyet

-- [Use OS crypt]

foreign import ccall "crypt" c_crypt :: CString -> CString -> IO CString

crypt :: String -> String -> String
crypt key salt =
  unsafePerformIO $ bracket (takeMVar lock) (putMVar lock) $ \_ ->
    withCString key $ \c_key -> withCString salt $ \c_salt ->
      c_crypt c_key c_salt >>= peekCString
  where lock = unsafePerformIO $ newMVar ()

bf_crypt (Str text : optional)
  | T.length salt < 2 = generateSalt >>= go
  | otherwise         = go salt
  where [Str salt] = defaults optional [Str ""]
        generateSalt = do
          c1 <- randSaltChar
          c2 <- randSaltChar
          return $ T.pack [c1, c2]
        randSaltChar = fmap (saltStuff !!) $
                       liftIO $ randomRIO (0, length saltStuff)
        saltStuff = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "./"
        go salt = return $ Str $ T.pack $ crypt (T.unpack text) (T.unpack salt)

-- [End crypt]

hash :: ByteString -> MOO Value
hash bs = return $ Str $ T.pack $ show md5hash
  where md5hash = MD5.hash' bs :: MD5Digest

bf_string_hash [Str text] = hash (encodeUtf8 text)

bf_binary_hash [Str bin_string] = binaryString bin_string >>= hash

-- 4.4.2.4 Operations on Lists

-- bf_length already defined above

bf_is_member [value, Lst list] =
  return $ Int $ maybe 0 (fromIntegral . succ) $
  V.findIndex (`equal` value) list

bf_listinsert [Lst list, value, Int index] = notyet
bf_listinsert [Lst list, value] = return $ Lst $ V.cons value list

bf_listappend [Lst list, value, Int index] = notyet
bf_listappend [Lst list, value] = return $ Lst $ V.snoc list value

bf_listdelete [Lst list, Int index]
  | index' < 1 || index' > V.length list = raise E_RANGE
  | otherwise = return $ Lst $ s V.++ V.tail r
  where index' = fromIntegral index
        (s, r) = V.splitAt (index' - 1) list

bf_listset [Lst list, value, Int index]
  | index' < 1 || index' > V.length list = raise E_RANGE
  | otherwise = return $ Lst $
                V.modify (\v -> VM.write v (index' - 1) value) list
  where index' = fromIntegral index

bf_setadd [Lst list, value] =
  return $ Lst $ if V.elem value list then list else V.snoc list value

bf_setremove [Lst list, value] =
  return $ Lst $ case V.elemIndex value list of
    Nothing    -> list
    Just index -> s V.++ V.tail r
      where (s, r) = V.splitAt index list
