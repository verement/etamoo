
module MOO.Builtins ( ) where

import qualified Data.Text     as T
import qualified Data.Vector   as V
import qualified System.Random as Random
import           MOO.Types

-- 4.4 Built-in Functions

type Builtin = [Value] -> IO Value

notimp = error "not yet implemented"

raise :: Error -> a
raise e = error $ T.unpack $ toText (Err e)

-- 4.4.1 Object-Oriented Programming

bf_pass_spec = (0, -1, [], TAny)
bf_pass :: Builtin
bf_pass = notimp

-- 4.4.2.1 General Operations Applicable to all Values

bf_typeof_spec = (1, 1, [TAny], TInt)
bf_typeof :: Builtin
bf_typeof = return . Int . typeCode . head

bf_tostr_spec = (0, -1, [], TStr)
bf_tostr :: Builtin
bf_tostr = return . Str . T.concat . map toText

bf_toliteral_spec = (1, 1, [TAny], TStr)
bf_toliteral :: Builtin
bf_toliteral = return . Str . toliteral' . head
  where toliteral' (List vs) = T.concat
                               ["{"
                               , T.intercalate ", " $
                                 map toliteral' (V.toList vs)
                               , "}"]
        toliteral' (Str x) = T.concat ["\"", T.concatMap escape x, "\""]
          where escape '"'  = "\\\""
                escape '\\' = "\\\\"
                escape c    = T.singleton c
        toliteral' (Err x) = T.pack $ show x
        toliteral' v = toText v

bf_toint_spec = (1, 1, [TAny], TInt)
bf_toint :: Builtin
bf_toint [v] = return $ case v of
  (Int  _) -> v
  (Real x) | x >= 0    -> Int $ floor   x
           | otherwise -> Int $ ceiling x
  (Obj  x) -> Int $ fromIntegral x
  (Str  x) -> notimp
  (Err  x) -> Int $ fromIntegral $ fromEnum x
  (List _) -> raise E_TYPE

bf_toobj_spec = (1, 1, [TAny], TObj)
bf_toobj :: Builtin
bf_toobj [v] = return $ case v of
  (Int  x) -> Obj $ fromIntegral x
  (Real x) | x >= 0    -> Obj $ floor   x
           | otherwise -> Obj $ ceiling x
  (Obj  _) -> v
  (Str  x) -> notimp
  (Err  x) -> Obj $ fromIntegral $ fromEnum x
  (List _) -> raise E_TYPE

bf_tofloat_spec = (1, 1, [TAny], TReal)
bf_tofloat :: Builtin
bf_tofloat [v] = return $ case v of
  (Int  x) -> Real $ fromIntegral x
  (Real _) -> v
  (Obj  x) -> Real $ fromIntegral x
  (Str  x) -> notimp
  (Err  x) -> Real $ fromIntegral $ fromEnum x
  (List _) -> raise E_TYPE

bf_equal_spec = (2, 2, [TAny, TAny], TInt)
bf_equal :: Builtin
bf_equal [v1, v2] = return $ truthValue (v1 == v2)

bf_value_bytes_spec = (1, 1, [TAny], TInt)
bf_value_bytes :: Builtin
bf_value_bytes [v] = notimp

bf_value_hash_spec = (1, 1, [TAny], TStr)
bf_value_hash :: Builtin
bf_value_hash [v] = do
  lit <- bf_toliteral [v]
  bf_string_hash [lit]

-- 4.4.2.2 Operations on Numbers

bf_random_spec = (0, 1, [TInt], TInt)
bf_random :: Builtin
bf_random []        = bf_random [Int maxBound]
bf_random [Int mod] = Random.randomRIO (1, mod) >>= return . Int

bf_min_spec = (1, -1, [TNum], TNum)
bf_min :: Builtin
bf_min (x@(Int  _):xs) = return $ minMaxInt  min x xs
bf_min (x@(Real _):xs) = return $ minMaxReal min x xs

bf_max_spec = (1, -1, [TNum], TNum)
bf_max :: Builtin
bf_max (x@(Int  _):xs) = return $ minMaxInt  max x xs
bf_max (x@(Real _):xs) = return $ minMaxReal max x xs

minMaxInt :: (IntType -> IntType -> IntType) -> Value -> [Value] -> Value
minMaxInt f = minMaxInt'
  where minMaxInt' v        []         = v
        minMaxInt' (Int x) (Int  y:rs) = minMaxInt' (Int $ f x y) rs
        minMaxInt' (Int _) (Real _:_)  = raise E_TYPE

minMaxReal :: (RealType -> RealType -> RealType) -> Value -> [Value] -> Value
minMaxReal f = minMaxReal'
  where minMaxReal' v        []          = v
        minMaxReal' (Real x) (Real y:rs) = minMaxReal' (Real $ f x y) rs
        minMaxReal' (Real _) (Int  _:_)  = raise E_TYPE

bf_abs_spec = (1, 1, [TNum], TNum)
bf_abs :: Builtin
bf_abs [Int  x] = return $ Int  $ abs x
bf_abs [Real x] = return $ Real $ abs x

-- ...

bf_string_hash :: Builtin
bf_string_hash = notimp
