
{-# LANGUAGE OverloadedStrings #-}

module MOO.Builtins.Values ( builtins ) where

import Control.Monad (mplus, unless, liftM)
import Data.ByteString (ByteString)
import Data.Char (intToDigit, isDigit)
import Data.Digest.Pure.MD5 (MD5Digest)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Text.Printf (printf)

import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import MOO.Types
import MOO.Task
import MOO.Parser (parseNum, parseObj)
import MOO.Builtins.Common
import MOO.Builtins.Crypt
import MOO.Builtins.Match

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- | § 4.4.2 Manipulating MOO Values
builtins :: [Builtin]
builtins = [
    -- § 4.4.2.1 General Operations Applicable to all Values
    bf_typeof
  , bf_tostr
  , bf_toliteral
  , bf_toint
  , bf_tonum
  , bf_toobj
  , bf_tofloat
  , bf_equal
  , bf_value_bytes
  , bf_value_hash

    -- § 4.4.2.2 Operations on Numbers
  , bf_random
  , bf_min
  , bf_max
  , bf_abs
  , bf_floatstr
  , bf_sqrt
  , bf_sin
  , bf_cos
  , bf_tan
  , bf_asin
  , bf_acos
  , bf_atan
  , bf_sinh
  , bf_cosh
  , bf_tanh
  , bf_exp
  , bf_log
  , bf_log10
  , bf_ceil
  , bf_floor
  , bf_trunc

    -- § 4.4.2.3 Operations on Strings
  , bf_length
  , bf_strsub
  , bf_index
  , bf_rindex
  , bf_strcmp
  , bf_decode_binary
  , bf_encode_binary
  , bf_match
  , bf_rmatch
  , bf_substitute
  , bf_crypt
  , bf_string_hash
  , bf_binary_hash

    -- § 4.4.2.4 Operations on Lists
  , bf_is_member
  , bf_listinsert
  , bf_listappend
  , bf_listdelete
  , bf_listset
  , bf_setadd
  , bf_setremove
  ]

-- § 4.4.2.1 General Operations Applicable to all Values

bf_typeof = Builtin "typeof" 1 (Just 1) [TAny] TInt $ \[value] ->
  return $ Int $ typeCode $ typeOf value

bf_tostr = Builtin "tostr" 0 Nothing [] TStr $ \values ->
  return $ Str $ T.concat $ map toText values

bf_toliteral = Builtin "toliteral" 1 (Just 1) [TAny] TStr $ \[value] ->
  return $ Str $ toLiteral value

-- XXX toint(" - 34  ") does not parse as -34
bf_toint = Builtin "toint" 1 (Just 1) [TAny] TInt $ \[value] -> toint value
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

bf_tonum = bf_toint { builtinName = "tonum" }

bf_toobj = Builtin "toobj" 1 (Just 1) [TAny] TObj $ \[value] -> toobj value
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

bf_tofloat = Builtin "tofloat" 1 (Just 1) [TAny] TFlt $
             \[value] -> tofloat value
  where tofloat value = case value of
          Int x -> return (Flt $ fromIntegral x)
          Flt _ -> return value
          Obj x -> return (Flt $ fromIntegral x)
          Str x -> maybe (return $ Flt 0) tofloat (parseNum x)
          Err x -> return (Flt $ fromIntegral $ fromEnum x)
          Lst _ -> raise E_TYPE

bf_equal = Builtin "equal" 2 (Just 2) [TAny, TAny] TInt $ \[value1, value2] ->
  return $ truthValue (value1 `equal` value2)

bf_value_bytes = Builtin "value_bytes" 1 (Just 1) [TAny] TInt $ \[value] ->
  return $ Int $ fromIntegral $ storageBytes value

bf_value_hash = Builtin "value_hash" 1 (Just 1) [TAny] TStr $ \[value] -> do
  literal <- builtinFunction bf_toliteral [value]
  builtinFunction bf_string_hash [literal]

-- § 4.4.2.2 Operations on Numbers

bf_random = Builtin "random" 0 (Just 1) [TInt] TInt go
  where go optional
          | mod <= 0  = raise E_INVARG
          | otherwise = Int `liftM` random (1, mod)
          where [Int mod] = defaults optional [Int maxBound]

bf_min = Builtin "min" 1 Nothing [TNum] TNum go
  where go (Int x:xs) = minMaxInt min x xs
        go (Flt x:xs) = minMaxFlt min x xs

bf_max = Builtin "max" 1 Nothing [TNum] TNum go
  where go (Int x:xs) = minMaxInt max x xs
        go (Flt x:xs) = minMaxFlt max x xs

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

bf_abs = Builtin "abs" 1 (Just 1) [TNum] TNum go
  where go [Int x] = return $ Int $ abs x
        go [Flt x] = return $ Flt $ abs x

bf_floatstr = Builtin "floatstr" 2 (Just 3) [TFlt, TInt, TAny] TStr go
  where go (Flt x : Int precision : optional)
          | precision < 0 = raise E_INVARG
          | otherwise = return $ Str $ T.pack $ printf format x
          where [scientific] = booleanDefaults optional [False]
                prec = min precision 19
                format = printf "%%.%d%c" prec $ if scientific then 'e' else 'f'

floatBuiltin :: Id -> ([Value] -> MOO Value) -> Builtin
floatBuiltin name = Builtin name 1 (Just 1) [TFlt] TFlt

bf_sqrt  = floatBuiltin "sqrt"  $ \[Flt x] -> checkFloat $ sqrt x

bf_sin   = floatBuiltin "sin"   $ \[Flt x] -> checkFloat $ sin x
bf_cos   = floatBuiltin "cos"   $ \[Flt x] -> checkFloat $ cos x
bf_tan   = floatBuiltin "tan"   $ \[Flt x] -> checkFloat $ tan x

bf_asin  = floatBuiltin "asin"  $ \[Flt x] -> checkFloat $ asin x
bf_acos  = floatBuiltin "acos"  $ \[Flt x] -> checkFloat $ acos x
bf_atan  = Builtin "atan" 1 (Just 2) [TFlt, TFlt] TFlt go
  where go [Flt y] = checkFloat $ atan y
        go [Flt y,
            Flt x] = checkFloat $ atan2 y x

bf_sinh  = floatBuiltin "sinh"  $ \[Flt x] -> checkFloat $ sinh x
bf_cosh  = floatBuiltin "cosh"  $ \[Flt x] -> checkFloat $ cosh x
bf_tanh  = floatBuiltin "tanh"  $ \[Flt x] -> checkFloat $ tanh x

bf_exp   = floatBuiltin "exp"   $ \[Flt x] -> checkFloat $ exp x
bf_log   = floatBuiltin "log"   $ \[Flt x] -> checkFloat $ log x
bf_log10 = floatBuiltin "log10" $ \[Flt x] -> checkFloat $ logBase 10 x

bf_ceil  = floatBuiltin "ceil"  $ \[Flt x] ->
  checkFloat $ fromIntegral (ceiling x :: Integer)
bf_floor = floatBuiltin "floor" $ \[Flt x] ->
  checkFloat $ fromIntegral (floor   x :: Integer)
bf_trunc = floatBuiltin "trunc" go
  where go [Flt x]
          | x < 0     = checkFloat $ fromIntegral (ceiling x :: Integer)
          | otherwise = checkFloat $ fromIntegral (floor   x :: Integer)

-- § 4.4.2.3 Operations on Strings

bf_length = Builtin "length" 1 (Just 1) [TAny] TInt go
  where go [Str string] = return $ Int $ fromIntegral $ T.length string
        go [Lst list]   = return $ Int $ fromIntegral $ V.length list
        go _            = raise E_TYPE

bf_strsub = Builtin "strsub" 3 (Just 4) [TStr, TStr, TStr, TAny] TStr go
  where go (Str subject : Str what : Str with : optional)
          | T.null what = raise E_INVARG
          | otherwise   = return $ Str $ T.concat $ subs subject
          where [case_matters] = booleanDefaults optional [False]
                caseFold str = if case_matters then str else T.toCaseFold str
                -- this won't work for Unicode in general
                subs "" = []
                subs subject = case T.breakOn what' (caseFold subject) of
                  (_, "")     -> [subject]
                  (prefix, _) ->
                    let (s, r) = T.splitAt (T.length prefix) subject
                    in s : with : subs (T.drop whatLen r)
                what' = caseFold what
                whatLen = T.length what

bf_index = Builtin "index" 2 (Just 3) [TStr, TStr, TAny] TInt go
  where go (Str str1 : Str str2 : optional)
          | T.null str2 = return (Int 1)
          | otherwise   =
            return $ Int $ case T.breakOn (caseFold str2) (caseFold str1) of
              (_, "")     -> 0
              (prefix, _) -> fromIntegral $ 1 + T.length prefix
          where [case_matters] = booleanDefaults optional [False]
                caseFold str = if case_matters then str else T.toCaseFold str
                -- this won't work for Unicode in general

bf_rindex = Builtin "rindex" 2 (Just 3) [TStr, TStr, TAny] TInt go
  where go (Str str1 : Str str2 : optional)
          | T.null str2 = return (Int $ fromIntegral $ T.length str1 + 1)
          | otherwise   =
            return $ Int $ case T.breakOnEnd needle haystack of
              ("", _)     -> 0
              (prefix, _) -> fromIntegral $
                             1 + T.length prefix - T.length needle
          where [case_matters] = booleanDefaults optional [False]
                needle   = caseFold str2
                haystack = caseFold str1
                caseFold str = if case_matters then str else T.toCaseFold str
                -- this won't work for Unicode in general

bf_strcmp = Builtin "strcmp" 2 (Just 2)
            [TStr, TStr] TInt $ \[Str str1, Str str2] ->
  return $ Int $ case compare str1 str2 of
    LT -> -1
    EQ ->  0
    GT ->  1

bf_decode_binary = Builtin "decode_binary" 1 (Just 2) [TStr, TAny] TLst go
  where go (Str bin_string : optional) =
          maybe (raise E_INVARG) (return . mkResult) $ text2binary bin_string

          where [fully] = booleanDefaults optional [False]

                mkResult | fully     = fromListBy (Int . fromIntegral)
                         | otherwise = fromList . groupPrinting ("" ++)

                groupPrinting g (w:ws)
                  | validStrChar c = groupPrinting (g [c] ++) ws
                  | null group     = Int (fromIntegral w) : groupPrinting g ws
                  | otherwise      = Str (T.pack group) : Int (fromIntegral w) :
                                     groupPrinting ("" ++) ws
                  where c = toEnum (fromIntegral w)
                        group = g ""
                groupPrinting g []
                  | null group = []
                  | otherwise  = [Str $ T.pack group]
                  where group = g ""

bf_encode_binary = Builtin "encode_binary" 0 Nothing [] TStr $
                   liftM (Str . T.pack) . encodeBinary

encodeBinary :: [Value] -> MOO String
encodeBinary (Int n : args)
  | n >= 0 && n <= 255 = prepend `liftM` encodeBinary args
  | otherwise          = raise E_INVARG
  where n'     = fromIntegral n
        c      = toEnum n'
        (q, r) = n' `divMod` 16
        prepend | validStrChar c &&
                  c /= '~' && c /= '\t' = (c :)
                | otherwise             = \rest -> '~' : hex q : hex r : rest
        hex = intToDigit  -- N.B. not uppercase
encodeBinary (Str str : args) = (encodeStr (T.unpack str) ++) `liftM`
                                encodeBinary args
  where encodeStr ('~' :cs) = "~7e" ++ encodeStr cs
        encodeStr ('\t':cs) = "~09" ++ encodeStr cs
        encodeStr (c   :cs) = c     :  encodeStr cs
        encodeStr ""        = ""
encodeBinary (Lst list : args) = do
  listEncoding <- encodeBinary (V.toList list)
  (listEncoding ++) `liftM` encodeBinary args
encodeBinary (_:_) = raise E_INVARG
encodeBinary []    = return ""

matchBuiltin :: Id -> (Regexp -> Text -> MatchResult) -> Builtin
matchBuiltin name matchFunc = Builtin name 2 (Just 3) [TStr, TStr, TAny] TLst go
  where go (Str subject : Str pattern : optional) =
          runMatch matchFunc subject pattern case_matters
          where [case_matters] = booleanDefaults optional [False]

bf_match  = matchBuiltin "match"  match
bf_rmatch = matchBuiltin "rmatch" rmatch

runMatch :: (Regexp -> Text -> MatchResult) ->
            StrT -> StrT -> Bool -> MOO Value
runMatch match subject pattern case_matters =
  case newRegexp pattern case_matters of
    Left (err, at) -> raiseException (Err E_INVARG)
                      (T.pack $ "Invalid pattern: " ++ err)
                      (Int $ fromIntegral at)
    Right regexp ->
      case match regexp subject of
        MatchFailed            -> return (Lst V.empty)
        MatchAborted           -> raise E_QUOTA
        MatchSucceeded offsets ->
          let (m : offs)   = offsets
              (start, end) = convert m
              replacements = repls 9 offs
          in return $ fromList
             [Int start, Int end, fromList replacements, Str subject]

  where -- convert from 0-based open interval to 1-based closed one
        convert (s,e)  = (1 + fromIntegral s, fromIntegral e)

        repls :: Int -> [(Int, Int)] -> [Value]
        repls n (r:rs) = let (s,e) = convert r
                         in fromList [Int s, Int e] : repls (n - 1) rs
        repls n []
          | n > 0      = fromList [Int 0, Int (-1)] : repls (n - 1) []
          | otherwise  = []

bf_substitute = Builtin "substitute" 2 (Just 2) [TStr, TLst] TStr go
  where go [Str template, Lst subs] =
          case V.toList subs of
            [Int start', Int end', Lst replacements', Str subject'] -> do
              let start      = fromIntegral start'
                  end        = fromIntegral end'
                  subject    = T.unpack subject'
                  subjectLen = T.length subject'

                  valid s e  = (s == 0 && e == -1) ||
                               (s >  0 && e >= s - 1 && e <= subjectLen)

                  substr start end =
                    let len = end - start + 1
                    in take len $ drop (start - 1) subject

                  substitution (Lst sub) = case V.toList sub of
                    [Int start', Int end'] -> do
                      let start = fromIntegral start'
                          end   = fromIntegral end'
                      unless (valid start end) $ raise E_INVARG
                      return $ substr start end
                    _ -> raise E_INVARG
                  substitution _ = raise E_INVARG

              unless (valid start end && V.length replacements' == 9) $
                raise E_INVARG
              replacements <- (substr start end :) `liftM`
                              mapM substitution (V.toList replacements')

              let walk ('%':c:cs)
                    | isDigit c = let i = fromEnum c - fromEnum '0'
                                  in (replacements !! i ++) `liftM` walk cs
                    | c == '%'  = ("%" ++) `liftM` walk cs
                    | otherwise = raise E_INVARG
                  walk (c:cs) = ([c] ++) `liftM` walk cs
                  walk []     = return []

              (Str . T.pack) `liftM` walk (T.unpack template)
            _ -> raise E_INVARG

bf_crypt = Builtin "crypt" 1 (Just 2) [TStr, TStr] TStr go
  where go (Str text : optional)
          | maybe True invalidSalt saltArg = generateSalt >>= go
          | otherwise                      = go $ fromStr $ fromJust saltArg
          where (saltArg : _) = maybeDefaults optional
                invalidSalt (Str salt) = salt `T.compareLength` 2 == LT
                generateSalt = do
                  c1 <- randSaltChar
                  c2 <- randSaltChar
                  return $ T.pack [c1, c2]
                randSaltChar = (saltStuff !!) `liftM`
                               random (0, length saltStuff - 1)
                saltStuff = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "./"
                go salt = case crypt (T.unpack text) (T.unpack salt) of
                  Just encrypted -> return $ Str $ T.pack encrypted
                  Nothing        -> raise E_QUOTA

hash :: ByteString -> Value
hash bs = Str $ T.pack $ show md5hash
  where md5hash = MD5.hash' bs :: MD5Digest

bf_string_hash = Builtin "string_hash" 1 (Just 1) [TStr] TStr go
  where go [Str text] = return $ hash $ encodeUtf8 text

bf_binary_hash = Builtin "binary_hash" 1 (Just 1) [TStr] TStr go
  where go [Str bin_string] = hash `liftM` binaryString bin_string

-- § 4.4.2.4 Operations on Lists

-- bf_length already defined above

bf_is_member = Builtin "is_member" 2 (Just 2) [TAny, TLst] TInt go
  where go [value, Lst list] =
          return $ Int $ maybe 0 (fromIntegral . succ) $
          V.findIndex (`equal` value) list

listInsert :: LstT -> Int -> Value -> LstT
listInsert list index value
  | index <= 0      = V.cons value list
  | index > listLen = V.snoc list value
  | otherwise       = V.create $ do
    list' <- V.thaw list >>= flip VM.grow 1
    let moveLen = listLen - index
        s = VM.slice  index      moveLen list'
        t = VM.slice (index + 1) moveLen list'
    VM.move t s
    VM.write list' index value
    return list'
  where listLen = V.length list

listDelete :: LstT -> Int -> LstT
listDelete list index
  | index == 0           = V.create $ VM.tail `liftM` V.thaw list
  | index == listLen - 1 = V.create $ VM.init `liftM` V.thaw list
  | otherwise            = V.create $ do
    list' <- V.thaw list
    let moveLen = listLen - index - 1
        s = VM.slice  index      moveLen list'
        t = VM.slice (index + 1) moveLen list'
    VM.move s t
    return $ VM.init list'
  where listLen = V.length list

bf_listinsert = Builtin "listinsert" 2 (Just 3) [TLst, TAny, TInt] TLst go
  where go (Lst list : value : optional) =
          return $ Lst $ listInsert list (fromIntegral index - 1) value
          where [Int index] = defaults optional [Int 1]

bf_listappend = Builtin "listappend" 2 (Just 3) [TLst, TAny, TInt] TLst go
  where go (Lst list : value : optional) =
          return $ Lst $ listInsert list (fromIntegral index) value
          where [Int index] = defaults optional
                              [Int $ fromIntegral $ V.length list]

bf_listdelete = Builtin "listdelete" 2 (Just 2) [TLst, TInt] TLst go
  where go [Lst list, Int index]
          | index' < 1 || index' > V.length list = raise E_RANGE
          | otherwise = return $ Lst $ listDelete list (index' - 1)
          where index' = fromIntegral index

bf_listset = Builtin "listset" 3 (Just 3) [TLst, TAny, TInt] TLst go
  where go [Lst list, value, Int index]
          | index' < 1 || index' > V.length list = raise E_RANGE
          | otherwise = return $ Lst $ listSet list index' value
          where index' = fromIntegral index

bf_setadd = Builtin "setadd" 2 (Just 2) [TLst, TAny] TLst go
  where go [Lst list, value] =
          return $ Lst $ if value `V.elem` list then list else V.snoc list value

bf_setremove = Builtin "setremove" 2 (Just 2) [TLst, TAny] TLst go
  where go [Lst list, value] =
          return $ Lst $ case V.elemIndex value list of
            Nothing    -> list
            Just index -> listDelete list (fromIntegral index)
