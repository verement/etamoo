
module MOO.Compiler ( compileExpr, catchException,
                      ExceptionHandler(..), Exception(..) ) where

import Control.Monad.Cont
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Vector as V
import MOO.Types
import MOO.AST

type MOO = ReaderT ExceptionHandler (ContT Value IO)

newtype ExceptionHandler = Handler (Exception -> MOO Value)

data Exception = Exception Code Message Value
type Code = Value
type Message = StrT

catchException :: MOO a -> (Exception -> MOO a) -> MOO a
catchException action handler = callCC $ \k -> local (mkHandler k) action
  where mkHandler k r = Handler $ \e -> local (const r) $ handler e >>= k

raiseException :: Exception -> MOO a
raiseException except = do
  Handler handler <- ask
  handler except
  error "Returned from exception handler"

raise :: Error -> MOO a
raise err = raiseException $ Exception (Err err) (error2text err) (Int 0)

compileExpr :: Expr -> MOO Value
compileExpr expr = case expr of
  Literal v -> liftIO (putStrLn $ "-- " ++ show v) >> return v
  List args -> mkList args

  Plus   a b -> binary plus   a b
  Minus  a b -> binary minus  a b
  Times  a b -> binary times  a b
  Divide a b -> binary divide a b
  Remain a b -> binary remain a b
  Power  a b -> binary power  a b
  Negate a   -> compileExpr a >>= negation

  Conditional c x y -> do
    c' <- compileExpr c
    compileExpr $ if truthOf c' then x else y
  And x y -> do
    x' <- compileExpr x
    if truthOf x'
      then compileExpr y
      else return x'
  Or x y -> do
    x' <- compileExpr x
    if truthOf x'
      then return x'
      else compileExpr y
  Not x -> fmap (truthValue . not . truthOf) $ compileExpr x

  Equal    x y -> binary (\a b -> return $ truthValue $ a == b) x y
  NotEqual x y -> binary (\a b -> return $ truthValue $ a /= b) x y

  LessThan     x y -> comparison (<)  x y
  LessEqual    x y -> comparison (<=) x y
  GreaterThan  x y -> comparison (>)  x y
  GreaterEqual x y -> comparison (>=) x y

  In m l -> do
    m' <- compileExpr m
    l' <- compileExpr l
    case l' of
      Lst v -> return $ Int $ maybe 0 (fromIntegral . succ) (V.elemIndex m' v)
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
        comparison op a b = do
          a' <- compileExpr a
          b' <- compileExpr b
          when (typeOf a' /= typeOf b') $ raise E_TYPE
          case a' of Lst{} -> raise E_TYPE
                     _     -> return $ truthValue (a' `op` b')

mkList :: [Arg] -> MOO Value
mkList args = fmap (Lst . V.fromList) $ expand args

expand :: [Arg] -> MOO [Value]
expand (a:as) = case a of
  ArgNormal expr -> do a' <- compileExpr expr
                       fmap (a' :) $ expand as
  ArgSplice expr -> do a' <- compileExpr expr
                       case a' of
                         Lst v -> fmap (V.toList v ++) $ expand as
                         _     -> raise E_TYPE
expand [] = return []

checkFloat :: FltT -> MOO Value
checkFloat flt | isInfinite flt = raise E_FLOAT
               | isNaN      flt = raise E_INVARG
               | otherwise      = return (Flt flt)

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

fmod :: FltT -> FltT -> FltT
x `fmod` y = x - fromIntegral n * y
  where n = roundZero (x / y)
        roundZero q | q > 0     = floor   q
                    | q < 0     = ceiling q
                    | otherwise = round   q

negation :: Value -> MOO Value
negation (Int a) = return $ Int (-a)
negation (Flt a) = return $ Flt (-a)
negation _       = raise E_TYPE
