
{-# LANGUAGE OverloadedStrings #-}

module MOO.Builtins.Values ( builtins ) where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad (unless, (<=<))
import Data.ByteString (ByteString)
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Text.Printf (printf)

import qualified Data.ByteString as BS
import qualified Data.Text as T

import MOO.Builtins.Common
import MOO.Builtins.Crypt
import MOO.Builtins.Hash
import MOO.Builtins.Match
import MOO.Parser (parseNum, parseObj)
import MOO.Task
import MOO.Types

import qualified MOO.List as Lst
import qualified MOO.String as Str

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

bf_tostr = Builtin "tostr" 0 Nothing [] TStr $
           return . Str . Str.fromText . T.concat . map toText

bf_toliteral = Builtin "toliteral" 1 (Just 1) [TAny] TStr $ \[value] ->
  return $ Str $ Str.fromText $ toLiteral value

-- XXX toint(" - 34  ") does not parse as -34
bf_toint = Builtin "toint" 1 (Just 1) [TAny] TInt $ \[value] -> toint value
  where toint value = case value of
          Int _ -> return value
          Flt x | x < fromIntegral (minBound :: IntT) ||
                  x > fromIntegral (maxBound :: IntT) -> raise E_FLOAT
                | otherwise -> return (Int $ truncate x)
          Obj x -> return (Int $ fromIntegral x)
          Str x -> maybe (return $ Int 0) toint (parseNum $ Str.toText x)
          Err x -> return (Int $ fromIntegral $ fromEnum x)
          Lst _ -> raise E_TYPE

bf_tonum = bf_toint { builtinName = "tonum" }

bf_toobj = Builtin "toobj" 1 (Just 1) [TAny] TObj $ \[value] -> toobj value
  where toobj value = case value of
          Int x -> return (Obj $ fromIntegral x)
          Flt x | x < fromIntegral (minBound :: ObjT) ||
                  x > fromIntegral (maxBound :: ObjT) -> raise E_FLOAT
                | otherwise -> return (Obj $ truncate x)
          Obj _ -> return value
          Str x -> maybe (return $ Obj 0) toobj $
                   parseNum (Str.toText x) <|> parseObj (Str.toText x)
          Err x -> return (Obj $ fromIntegral $ fromEnum x)
          Lst _ -> raise E_TYPE

bf_tofloat = Builtin "tofloat" 1 (Just 1)
             [TAny] TFlt $ \[value] -> tofloat value
  where tofloat value = case value of
          Int x -> return (Flt $ fromIntegral x)
          Flt _ -> return value
          Obj x -> return (Flt $ fromIntegral x)
          Str x -> maybe (return $ Flt 0) tofloat (parseNum $ Str.toText x)
          Err x -> return (Flt $ fromIntegral $ fromEnum x)
          Lst _ -> raise E_TYPE

bf_equal = Builtin "equal" 2 (Just 2) [TAny, TAny] TInt $ \[value1, value2] ->
  return $ truthValue (value1 `equal` value2)

bf_value_bytes = Builtin "value_bytes" 1 (Just 1) [TAny] TInt $ \[value] ->
  return $ Int $ fromIntegral $ storageBytes value

bf_value_hash = Builtin "value_hash" 1 (Just 3)
                [TAny, TStr, TAny] TStr $ \(value : optional) ->
  builtinFunction bf_toliteral [value] >>=
  builtinFunction bf_string_hash . (: optional)

-- § 4.4.2.2 Operations on Numbers

bf_random = Builtin "random" 0 (Just 1) [TInt] TInt $ \optional ->
  let [Int mod] = defaults optional [Int maxBound]
  in if mod < 1 then raise E_INVARG
     else Int <$> random (1, mod)

bf_min = Builtin "min" 1 Nothing [TNum] TNum $ \args -> case args of
  Int x:xs -> minMaxInt min x xs
  Flt x:xs -> minMaxFlt min x xs

bf_max = Builtin "max" 1 Nothing [TNum] TNum $ \args -> case args of
  Int x:xs -> minMaxInt max x xs
  Flt x:xs -> minMaxFlt max x xs

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

bf_abs = Builtin "abs" 1 (Just 1) [TNum] TNum $ \[arg] -> case arg of
  Int x -> return $ Int $ abs x
  Flt x -> return $ Flt $ abs x

bf_floatstr = Builtin "floatstr" 2 (Just 3)
              [TFlt, TInt, TAny] TStr $ \(Flt x : Int precision : optional) ->
  let [scientific] = booleanDefaults optional [False]
      prec = min precision 19
      format = printf "%%.%d%c" prec $ if scientific then 'e' else 'f'
  in if precision < 0 then raise E_INVARG
     else return $ Str $ Str.fromString $ printf format x

floatBuiltin :: Id -> (FltT -> FltT) -> Builtin
floatBuiltin name f = Builtin name 1 (Just 1)
                      [TFlt] TFlt $ \[Flt x] -> checkFloat (f x)

bf_sqrt  = floatBuiltin "sqrt" sqrt

bf_sin   = floatBuiltin "sin"  sin
bf_cos   = floatBuiltin "cos"  cos
bf_tan   = floatBuiltin "tan"  tan

bf_asin  = floatBuiltin "asin" asin
bf_acos  = floatBuiltin "acos" acos
bf_atan  = Builtin "atan" 1 (Just 2) [TFlt, TFlt] TFlt $ \args ->
  checkFloat $ case args of
    [Flt y]        -> atan  y
    [Flt y, Flt x] -> atan2 y x

bf_sinh  = floatBuiltin "sinh" sinh
bf_cosh  = floatBuiltin "cosh" cosh
bf_tanh  = floatBuiltin "tanh" tanh

bf_exp   = floatBuiltin "exp"  exp
bf_log   = floatBuiltin "log"  log
bf_log10 = floatBuiltin "log10" (logBase 10)

bf_ceil  = floatBuiltin "ceil"  $ fromInteger . ceiling
bf_floor = floatBuiltin "floor" $ fromInteger . floor
bf_trunc = floatBuiltin "trunc" $ fromInteger . truncate

-- § 4.4.2.3 Operations on Strings

bf_length = Builtin "length" 1 (Just 1) [TAny] TInt $ \[arg] -> case arg of
  Str string -> return $ Int $ fromIntegral $ Str.length string
  Lst list   -> return $ Int $ fromIntegral $ Lst.length list
  _          -> raise E_TYPE

caseFold' :: Bool -> StrT -> StrT
caseFold' caseMatters
  | caseMatters = id
  | otherwise   = Str.fromText . Str.toCaseFold
                  -- XXX this won't work for Unicode in general

bf_strsub = Builtin "strsub" 3 (Just 4) [TStr, TStr, TStr, TAny]
            TStr $ \(Str subject : Str what : Str with : optional) ->
  let [case_matters] = booleanDefaults optional [False]
      caseFold = caseFold' case_matters :: StrT -> StrT
      what'    = caseFold what          :: StrT

      subs :: StrT -> [StrT]
      subs ""      = []
      subs subject = case Str.breakOn what' (caseFold subject) of
        (_, "")     -> [subject]
        (prefix, _) -> let (s, r) = Str.splitAt (Str.length prefix) subject
                       in s : with : subs (Str.drop whatLen r)

      whatLen :: Int
      whatLen = Str.length what

  in if Str.null what then raise E_INVARG
     else return $ Str $ Str.concat $ subs subject

indexBuiltin :: Id -> (StrT -> IntT) -> (StrT -> StrT -> IntT) -> Builtin
indexBuiltin name nullCase mainCase =
  Builtin name 2 (Just 3) [TStr, TStr, TAny]
  TInt $ \(Str str1 : Str str2 : optional) ->
  let [case_matters] = booleanDefaults optional [False]
      caseFold = caseFold' case_matters :: StrT -> StrT

  in return $ Int $ if Str.null str2 then nullCase str1
                    else mainCase (caseFold str2) (caseFold str1)

bf_index = indexBuiltin "index" nullCase mainCase
  where nullCase = const 1
        mainCase needle haystack = case Str.breakOn needle haystack of
          (_, "")     -> 0
          (prefix, _) -> fromIntegral $ 1 + Str.length prefix

bf_rindex = indexBuiltin "rindex" nullCase mainCase
  where nullCase haystack = fromIntegral $ Str.length haystack + 1
        mainCase needle haystack = case Str.breakOnEnd needle haystack of
          ("", _)     -> 0
          (prefix, _) -> fromIntegral $
                         1 + Str.length prefix - Str.length needle

bf_strcmp = Builtin "strcmp" 2 (Just 2)
            [TStr, TStr] TInt $ \[Str str1, Str str2] ->
  return $ Int $ case Str.toText str1 `compare` Str.toText str2 of
    LT -> -1
    EQ ->  0
    GT ->  1

bf_decode_binary = Builtin "decode_binary" 1 (Just 2)
                   [TStr, TAny] TLst $ \(Str bin_string : optional) ->
  let [fully] = booleanDefaults optional [False]

      mkResult :: [Word8] -> Value
      mkResult | fully     = fromListBy (Int . fromIntegral)
               | otherwise = fromList . grouping id

      grouping :: (String -> String) -> [Word8] -> [Value]
      grouping g (w:ws)
        | Str.validChar c = grouping (g . (c :)) ws
        | otherwise       = group g ++ Int (fromIntegral w) : grouping id ws
        where c = toEnum (fromIntegral w)
      grouping g [] = group g

      group :: (String -> String) -> [Value]
      group g = [ Str (Str.fromString g') | let g' = g [], not (null g') ]

  in mkResult . BS.unpack <$> binaryString bin_string

bf_encode_binary = Builtin "encode_binary" 0 Nothing [] TStr $
                   fmap (Str . Str.fromBinary) . encodeBinary

encodeBinary :: [Value] -> MOO ByteString
encodeBinary = maybe (raise E_INVARG) (return . BS.pack) . encode
  where encode :: [Value] -> Maybe [Word8]
        encode (Int n : rest)
          | n >= 0 && n <= 255 = (fromIntegral n :)             <$> encode rest
          | otherwise          = Nothing
        encode (Str s : rest)  = (++) <$> encodeStr s           <*> encode rest
        encode (Lst v : rest)  = (++) <$> encode (Lst.toList v) <*> encode rest
        encode (_     : _   )  = Nothing
        encode []              = Just []

        encodeStr :: StrT -> Maybe [Word8]
        encodeStr = mapM encodeChar . Str.toString

        encodeChar :: Char -> Maybe Word8
        encodeChar c
          | n >= 0 && n <= 255 = Just (fromIntegral n)
          | otherwise          = Nothing
          where n = fromEnum c

matchBuiltin :: Id -> (Regexp -> Text -> MatchResult) -> Builtin
matchBuiltin name matchFunc = Builtin name 2 (Just 3) [TStr, TStr, TAny]
                              TLst $ \(Str subject : Str pattern : optional) ->
  let [case_matters] = booleanDefaults optional [False]
  in runMatch matchFunc subject pattern case_matters

bf_match  = matchBuiltin  "match"  match
bf_rmatch = matchBuiltin "rmatch" rmatch

runMatch :: (Regexp -> Text -> MatchResult) -> StrT -> StrT -> Bool -> MOO Value
runMatch match subject pattern caseMatters =
  case Str.toRegexp caseMatters pattern of
    Right regexp -> case regexp `match` Str.toText subject of
      MatchSucceeded offsets ->
        let (m : offs)   = offsets
            (start, end) = convert m
            replacements = repls 9 offs
        in return $ fromList
           [Int start, Int end, fromList replacements, Str subject]
      MatchFailed  -> return emptyList
      MatchAborted -> raise E_QUOTA
    Left err -> let message = Str.fromString $ "Invalid pattern: " ++ err
                in raiseException (Err E_INVARG) message zero

  where convert :: (Int, Int) -> (IntT, IntT)
        convert (s, e) = (1 + fromIntegral s, fromIntegral e)
        -- convert from 0-based open interval to 1-based closed one

        repls :: Int -> [(Int, Int)] -> [Value]
        repls n (r:rs) = let (s,e) = convert r
                         in fromList [Int s, Int e] : repls (n - 1) rs
        repls n []
          | n > 0      = fromList [Int 0, Int (-1)] : repls (n - 1) []
          | otherwise  = []

bf_substitute = Builtin "substitute" 2 (Just 2)
                [TStr, TLst] TStr $ \[Str template, Lst subs] ->
  case Lst.toList subs of
    [Int start', Int end', Lst replacements', Str subject'] -> do
      let start      = fromIntegral start'   :: Int
          end        = fromIntegral end'     :: Int
          subject    = Str.toString subject' :: String
          subjectLen = Str.length subject'   :: Int

          valid :: Int -> Int -> Bool
          valid s e  = (s == 0 && e == -1) ||
                       (s >  0 && e >= s - 1 && e <= subjectLen)

          substr :: Int -> Int -> String
          substr start end =
            let len = end - start + 1
            in take len $ drop (start - 1) subject

          substitution :: Value -> MOO String
          substitution (Lst sub) = case Lst.toList sub of
            [Int start', Int end'] -> do
              let start = fromIntegral start'
                  end   = fromIntegral end'
              unless (valid start end) $ raise E_INVARG
              return $ substr start end
            _ -> raise E_INVARG
          substitution _ = raise E_INVARG

      unless (valid start end && Lst.length replacements' == 9) $ raise E_INVARG
      replacements <- (substr start end :) <$>
                      mapM substitution (Lst.toList replacements')

      let walk :: String -> MOO String
          walk ('%':c:cs)
            | isDigit c = let i = fromEnum c - fromEnum '0'
                          in (replacements !! i ++) <$> walk cs
            | c == '%'  = (c :) <$> walk cs
            | otherwise = raise E_INVARG
          walk (c:cs) = (c :) <$> walk cs
          walk []     = return []

      Str . Str.fromString <$> walk (Str.toString template)

    _ -> raise E_INVARG

bf_crypt = Builtin "crypt" 1 (Just 2)
           [TStr, TStr] TStr $ \(Str text : optional) ->
  let (saltArg : _) = maybeDefaults optional
      go salt = do
        result <- unsafeIOtoMOO $ crypt (Str.toString text) (Str.toString salt)
        maybe (raise E_INVARG) (return . Str . Str.fromString) result
  in if maybe True invalidSalt saltArg
     then generateSalt >>= go
     else go $ fromStr $ fromJust saltArg

  where invalidSalt :: Value -> Bool
        invalidSalt (Str salt) = salt `Str.compareLength` 2 == LT

        generateSalt :: MOO StrT
        generateSalt = do
          c1 <- randSaltChar
          c2 <- randSaltChar
          return $ Str.fromString [c1, c2]

        randSaltChar :: MOO Char
        randSaltChar = (saltStuff !!) <$> random (0, length saltStuff - 1)

        saltStuff :: String
        saltStuff = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "./"

hashBuiltin :: Id -> ((ByteString -> MOO Value) -> StrT -> MOO Value) -> Builtin
hashBuiltin name f = Builtin name 1 (Just 3)
                     [TStr, TStr, TAny] TStr $ \(Str input : optional) ->
  let (Str algorithm : optional') =        defaults optional  [Str "MD5"]
      [wantBinary]                = booleanDefaults optional' [False]
  in f (hash algorithm wantBinary) input

  where hash :: StrT -> Bool -> ByteString -> MOO Value
        hash alg wantBinary bytes =
          case hashBytesUsing (toId alg) wantBinary bytes of
            Just digest -> return (Str digest)
            Nothing     -> let message = "Unknown hash algorithm: " <> alg
                           in raiseException (Err E_INVARG) message (Str alg)

bf_string_hash = hashBuiltin "string_hash" $
                 \hash -> hash . encodeUtf8 . Str.toText  -- XXX Unicode

bf_binary_hash = hashBuiltin "binary_hash" (<=< binaryString)

-- § 4.4.2.4 Operations on Lists

-- bf_length already defined above

bf_is_member = Builtin "is_member" 2 (Just 2)
               [TAny, TLst] TInt $ \[value, Lst list] ->
  return $ Int $ maybe 0 (fromIntegral . succ) $
  Lst.findIndex (`equal` value) list

bf_listinsert = Builtin "listinsert" 2 (Just 3)
                [TLst, TAny, TInt] TLst $ \(Lst list : value : optional) ->
  let [Int index] = defaults optional [Int 1]
  in return $ Lst $ Lst.insert list (fromIntegral index - 1) value

bf_listappend = Builtin "listappend" 2 (Just 3)
                [TLst, TAny, TInt] TLst $ \(Lst list : value : optional) ->
  let [Int index] = defaults optional [Int $ fromIntegral $ Lst.length list]
  in return $ Lst $ Lst.insert list (fromIntegral index) value

bf_listdelete = Builtin "listdelete" 2 (Just 2) [TLst, TAny]
                TLst $ \[Lst list, index] -> case index of
  Int index | index' < 1 || index' > Lst.length list -> raise E_RANGE
            | otherwise -> return $ Lst $ Lst.delete list (index' - 1)
    where index' = fromIntegral index
  Str key -> case Lst.assocLens key list of
    Just (_, change) -> return (Lst $ change Nothing)
    Nothing          -> raise E_INVIND
  _ -> raise E_TYPE

bf_listset = Builtin "listset" 3 (Just 3) [TLst, TAny, TAny]
             TLst $ \[Lst list, value, index] -> case index of
  Int index | index' < 1 || index' > Lst.length list -> raise E_RANGE
            | otherwise -> return $ Lst $ Lst.set list (index' - 1) value
    where index' = fromIntegral index
  Str key -> case Lst.assocLens key list of
    Just (_, change) -> return (Lst $ change $ Just value)
    Nothing          -> raise E_INVIND
  _ -> raise E_TYPE

bf_setadd = Builtin "setadd" 2 (Just 2)
            [TLst, TAny] TLst $ \[Lst list, value] ->
  return $ Lst $ if value `Lst.elem` list then list else Lst.snoc list value

bf_setremove = Builtin "setremove" 2 (Just 2)
               [TLst, TAny] TLst $ \[Lst list, value] ->
  return $ Lst $ maybe list (Lst.delete list) $ value `Lst.elemIndex` list
