
{-# LANGUAGE OverloadedStrings #-}

module MOO.Compiler ( compileExpr ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, void)

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map as Map

import MOO.Types
import MOO.AST
import MOO.Task
import MOO.Builtins

catchDebug :: MOO Value -> MOO Value
catchDebug action = action `catchException` \except@(Exception code _ _) -> do
  debug <- frame debugBit
  if debug then raiseException except else return code

compileExpr :: Expr -> MOO Value
compileExpr expr = catchDebug $ case expr of
  Literal v -> return v
  List args -> fmap (Lst . V.fromList) $ expand args

  Variable{} -> fetch (lValue expr)
  PropRef{}  -> fetch (lValue expr)

  Assign what expr -> store (lValue what) =<< compileExpr expr

  ScatterAssign items expr -> do
    expr' <- compileExpr expr
    case expr' of
      Lst v -> scatterAssign items v
      _     -> raise E_TYPE

  VerbCall{} -> notyet

  BuiltinFunc func args -> callBuiltin (T.toCaseFold func) =<< expand args

  a `Plus`   b -> binary plus   a b
  a `Minus`  b -> binary minus  a b
  a `Times`  b -> binary times  a b
  a `Divide` b -> binary divide a b
  a `Remain` b -> binary remain a b
  a `Power`  b -> binary power  a b

  Negate a -> do a' <- compileExpr a
                 case a' of
                   Int x -> return (Int $ negate x)
                   Flt x -> return (Flt $ negate x)
                   _     -> raise E_TYPE

  Conditional c x y -> do c' <- compileExpr c
                          compileExpr $ if truthOf c' then x else y

  x `And` y -> do x' <- compileExpr x
                  if truthOf x' then compileExpr y else return x'
  x `Or`  y -> do x' <- compileExpr x
                  if truthOf x' then return x' else compileExpr y

  Not x -> fmap (truthValue . not . truthOf) $ compileExpr x

  x `Equal`        y -> equality   (==) x y
  x `NotEqual`     y -> equality   (/=) x y
  x `LessThan`     y -> comparison (<)  x y
  x `LessEqual`    y -> comparison (<=) x y
  x `GreaterThan`  y -> comparison (>)  x y
  x `GreaterEqual` y -> comparison (>=) x y

  Index{} -> fetch (lValue expr)
  Range{} -> fetch (lValue expr)

  Length -> fmap (Int . fromIntegral) =<< reader indexLength

  item `In` list -> do
    item' <- compileExpr item
    list' <- compileExpr list
    case list' of
      Lst v -> return $ Int $ maybe 0 (fromIntegral . succ) $
               V.elemIndex item' v
      _     -> raise E_TYPE

  Catch expr codes (Default dv) -> do
    codes' <- case codes of
      ANY        -> return Nothing
      Codes args -> fmap Just (expand args)
    compileExpr expr `catchException` \except@(Exception code _ _) ->
      if maybe True (code `elem`) codes'
        then maybe (return code) compileExpr dv
        else raiseException except

  where binary op a b = do
          a' <- compileExpr a
          b' <- compileExpr b
          a' `op` b'
        equality op = binary test
          where test a b = return $ truthValue (a `op` b)
        comparison op = binary test
          where test a b = do when (typeOf a /= typeOf b) $ raise E_TYPE
                              case a of
                                Lst{} -> raise E_TYPE
                                _     -> return $ truthValue (a `op` b)

getVariable :: Id -> MOO Value
getVariable var = do
  vars <- frame variables
  maybe (raise E_VARNF) return $ Map.lookup var vars

putVariable :: Id -> Value -> MOO Value
putVariable var value = do
  modifyFrame $ \frame -> frame {
    variables = Map.insert var value (variables frame)
    }
  return value

withIndexLength :: Value -> MOO a -> MOO a
withIndexLength expr =
  local $ \env -> env { indexLength = case expr of
                           Lst v -> return (V.length v)
                           Str t -> return (T.length t)
                           _     -> raise E_TYPE
                      }

checkLstRange :: LstT -> Int -> MOO ()
checkLstRange v i = when (i < 1 || i > V.length v) $ raise E_RANGE

checkStrRange :: StrT -> Int -> MOO ()
checkStrRange t i = when (i < 1 || t `T.compareLength` i == LT) $ raise E_RANGE

checkIndex :: Value -> MOO Int
checkIndex (Int i) = return (fromIntegral i)
checkIndex _       = raise E_TYPE

data LValue = LValue {
    fetch  :: MOO Value
  , store  :: Value -> MOO Value
  , change :: MOO (Value, Value -> MOO Value)
  }

lValue :: Expr -> LValue

lValue (Variable var) = LValue fetch store change
  where var'  = T.toCaseFold var
        fetch = getVariable var'
        store = putVariable var'

        change = do
          value <- fetch
          return (value, store)

lValue PropRef{} = LValue notyet (const notyet) notyet

lValue (expr `Index` index) = LValue fetchIndex storeIndex changeIndex
  where fetchIndex = do
          (value, _) <- changeIndex
          return value

        storeIndex newValue = do
          (_, change) <- changeIndex
          change newValue
          return newValue

        changeIndex = do
          (value, changeExpr) <- change (lValue expr)
          index' <- checkIndex =<< withIndexLength value (compileExpr index)
          value' <- case value of
            Lst v -> checkLstRange v index' >> return (v V.! (index' - 1))
            Str t -> checkStrRange t index' >>
                     return (Str $ T.singleton $ t `T.index` (index' - 1))
            _     -> raise E_TYPE
          return (value', changeValue value index' changeExpr)

        changeValue (Lst v) index changeExpr newValue =
          changeExpr $ Lst $ listSet v index newValue

        changeValue (Str t) index changeExpr (Str c) = do
          when (c `T.compareLength` 1 /= EQ) $ raise E_INVARG
          let (s, r) = T.splitAt (index - 1) t
          changeExpr $ Str $ T.concat [s, c, T.tail r]

        changeValue _ _ _ _ = raise E_TYPE

lValue (expr `Range` (start, end)) = LValue fetchRange storeRange changeRange
  where fetchRange = do
          value <- fetch (lValue expr)
          (start', end') <- getIndices value
          if start' > end'
            then case value of
              Lst{} -> return $ Lst V.empty
              Str{} -> return $ Str T.empty
              _     -> raise E_TYPE
            else let len = end' - start' + 1 in case value of
              Lst v -> do checkLstRange v start' >> checkLstRange v end'
                          return $ Lst $ V.slice (start' - 1) len v
              Str t -> do checkStrRange t start' >> checkStrRange t end'
                          return $ Str $ T.take len $ T.drop (start' - 1) t
              _     -> raise E_TYPE

        getIndices value = withIndexLength value $ do
          start' <- checkIndex =<< compileExpr start
          end'   <- checkIndex =<< compileExpr end
          return (start', end')

        storeRange newValue = do
          (value, changeExpr) <- change (lValue expr)
          (start', end') <- getIndices value
          changeValue value start' end' changeExpr newValue
          return newValue

        changeValue (Lst v) start end changeExpr (Lst r) = do
          let len = V.length v
          when (end < 0 || start > len + 1) $ raise E_RANGE
          let pre  = sublist v 1 (start - 1)
              post = sublist v (end + 1) len
              sublist v s e
                | e < s     = V.empty
                | otherwise = V.slice (s - 1) (e - s + 1) v
          changeExpr $ Lst $ V.concat [pre, r, post]

        changeValue (Str t) start end changeExpr (Str r) = do
          when (end < 0 ||
                t `T.compareLength` (start - 1) == LT) $ raise E_RANGE
          let pre  = substr t 1 (start - 1)
              post = substr t (end + 1) (T.length t)
              substr t s e
                | e < s     = T.empty
                | otherwise = T.take (e - s + 1) $ T.drop (s - 1) t
          changeExpr $ Str $ T.concat [pre, r, post]

        changeValue _ _ _ _ _ = raise E_TYPE

        changeRange = error "Illegal Range as lvalue subexpression"

lValue expr = LValue fetch store change
  where fetch = compileExpr expr
        store _ = error "Unmodifiable LValue"

        change = do
          value <- fetch
          return (value, store)

scatterAssign :: [ScatItem] -> LstT -> MOO Value
scatterAssign items args = do
  when (nargs < nreqs || (not haveRest && nargs > ntarg)) $ raise E_ARGS
  walk items args (nargs - nreqs)
  return (Lst args)

  where nargs = V.length args
        nreqs = count required items
        nopts = count optional items
        ntarg = nreqs + nopts
        nrest = if haveRest && nargs >= ntarg then nargs - ntarg else 0

        haveRest = any rest items
        count p = length . filter p

        required ScatRequired{} = True
        required _              = False
        optional ScatOptional{} = True
        optional _              = False
        rest     ScatRest{}     = True
        rest     _              = False

        walk (item:items) args noptAvail =
          case item of
            ScatRequired var -> do
              assign var (V.head args)
              walk items (V.tail args) noptAvail
            ScatOptional var opt
              | noptAvail > 0 -> do assign var (V.head args)
                                    walk items (V.tail args) (noptAvail - 1)
              | otherwise     -> do
                case opt of Nothing   -> return ()
                            Just expr -> void $ compileExpr expr >>= assign var
                walk items args noptAvail
            ScatRest var -> do
              let (s, r) = V.splitAt nrest args
              assign var (Lst s)
              walk items r noptAvail
        walk [] _ _ = return ()

        assign var = putVariable (T.toCaseFold var)

expand :: [Arg] -> MOO [Value]
expand (a:as) = case a of
  ArgNormal expr -> do a' <- compileExpr expr
                       fmap (a' :) $ expand as
  ArgSplice expr -> do a' <- compileExpr expr
                       case a' of
                         Lst v -> fmap (V.toList v ++) $ expand as
                         _     -> raise E_TYPE
expand [] = return []

plus :: Value -> Value -> MOO Value
(Int a) `plus` (Int b) = return $ Int (a + b)
(Flt a) `plus` (Flt b) = checkFloat (a + b)
(Str a) `plus` (Str b) = return $ Str (T.append a b)
_       `plus` _       = raise E_TYPE

minus :: Value -> Value -> MOO Value
(Int a) `minus` (Int b) = return $ Int (a - b)
(Flt a) `minus` (Flt b) = checkFloat (a - b)
_       `minus` _       = raise E_TYPE

times :: Value -> Value -> MOO Value
(Int a) `times` (Int b) = return $ Int (a * b)
(Flt a) `times` (Flt b) = checkFloat (a * b)
_       `times` _       = raise E_TYPE

divide :: Value -> Value -> MOO Value
(Int a) `divide` (Int b) | b == 0    = raise E_DIV
                         | otherwise = return $ Int (a `quot` b)
(Flt a) `divide` (Flt b) | b == 0    = raise E_DIV
                         | otherwise = checkFloat (a / b)
_       `divide` _                   = raise E_TYPE

remain :: Value -> Value -> MOO Value
(Int a) `remain` (Int b) | b == 0    = raise E_DIV
                         | otherwise = return $ Int (a `rem` b)
(Flt a) `remain` (Flt b) | b == 0    = raise E_DIV
                         | otherwise = checkFloat (a `fmod` b)
_       `remain` _                   = raise E_TYPE

fmod :: FltT -> FltT -> FltT
x `fmod` y = x - fromIntegral n * y
  where n = roundZero (x / y)
        roundZero q | q > 0     = floor   q
                    | q < 0     = ceiling q
                    | otherwise = round   q

power :: Value -> Value -> MOO Value
(Int a) `power` (Int b)
  | b >= 0    = return $ Int (a ^ b)
  | otherwise = case a of
    -1 | even b    -> return $ Int   1
       | otherwise -> return $ Int (-1)
    0 -> raise E_DIV
    1 -> return (Int 1)
    _ -> return (Int 0)

(Flt a) `power` (Int b) | b >= 0    = checkFloat (a ^ b)
                        | otherwise = checkFloat (a ** fromIntegral b)
(Flt a) `power` (Flt b) = checkFloat (a ** b)
_       `power` _       = raise E_TYPE
