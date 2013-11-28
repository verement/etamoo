
{-# LANGUAGE OverloadedStrings #-}

module MOO.Compiler ( compileExpr, catchException, initEnvironment, initStack
                    , ExceptionHandler(..), Exception(..) ) where

import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow (first)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Map (Map)
import qualified Data.Map as Map
import MOO.Types
import MOO.AST

type MOO = ReaderT Environment (StateT CallStack (ContT Value IO))

newtype CallStack = Stack [StackFrame]

data StackFrame =
  Frame { variables :: Map Id Value
        , debugBit  :: Bool
        }

currentFrame :: CallStack -> StackFrame
currentFrame (Stack (x:_)) = x
currentFrame (Stack  [])   = error "Empty call stack"

frame :: (StackFrame -> a) -> MOO a
frame f = gets (f . currentFrame)

modifyFrame :: (StackFrame -> StackFrame) -> MOO ()
modifyFrame f = modify $ \(Stack (frame:stack)) -> Stack (f frame : stack)

initStack :: CallStack
initStack = Stack [Frame { variables = mkInitVars
                         , debugBit  = True
                         }]

mkInitVars = Map.fromList $ map (first T.toCaseFold) initVars

initVars = [
    ("player" , Obj (-1))
  , ("this"   , Obj (-1))
  , ("caller" , Obj (-1))

  , ("args"   , Lst V.empty)
  , ("argstr" , Str T.empty)

  , ("verb"   , Str T.empty)
  , ("dobjstr", Str T.empty)
  , ("dobj"   , Obj (-1))
  , ("prepstr", Str T.empty)
  , ("iobjstr", Str T.empty)
  , ("iobj"   , Obj (-1))
  ] ++ typeVars

typeVars = [
    ("INT"  , Int $ typeCode TInt)
  , ("NUM"  , Int $ typeCode TInt)
  , ("FLOAT", Int $ typeCode TFlt)
  , ("LIST" , Int $ typeCode TLst)
  , ("STR"  , Int $ typeCode TStr)
  , ("OBJ"  , Int $ typeCode TObj)
  , ("ERR"  , Int $ typeCode TErr)
  ]

data Environment =
  Env { exceptionHandler :: ExceptionHandler
      , indexLength      :: MOO Int
      }

initEnvironment :: Environment
initEnvironment = Env {
    exceptionHandler = Handler $ \(Exception _ m _) -> error (T.unpack m)
  , indexLength      = error "invalid index context"
  }

newtype ExceptionHandler = Handler (Exception -> MOO Value)

data Exception = Exception Code Message Value
type Code = Value
type Message = StrT

catchException :: MOO a -> (Exception -> MOO a) -> MOO a
catchException action handler = callCC $ \k -> local (mkHandler k) action
  where mkHandler k r = r { exceptionHandler = Handler $ \e ->
                             local (const r) $ handler e >>= k }

raiseException :: Exception -> MOO a
raiseException except = do
  Handler handler <- reader exceptionHandler
  handler except
  error "Returned from exception handler"

raise :: Error -> MOO a
raise err = raiseException $ Exception (Err err) (error2text err) (Int 0)

notimp :: MOO a
notimp = raiseException $ Exception (Str notyet) notyet (Int 0)
  where notyet = "Not yet implemented"

catchDebug :: MOO Value -> MOO Value
catchDebug action = action `catchException` \except@(Exception code _ _) -> do
  debug <- frame debugBit
  if debug then raiseException except else return code

compileExpr :: Expr -> MOO Value
compileExpr expr = catchDebug $ case expr of
  Literal v -> liftIO (putStrLn $ "-- " ++ show v) >> return v
  List args -> mkList args

  Variable var -> do
    vars <- frame variables
    maybe (raise E_VARNF) return $ Map.lookup (T.toCaseFold var) vars

  PropRef{} -> notimp

  Assign (Variable var) expr -> do
    value <- compileExpr expr
    assign var value

  Assign _ _ -> notimp

  ScatterAssign items expr -> do
    expr' <- compileExpr expr
    case expr' of
      Lst v -> scatterAssign items v
      _     -> raise E_TYPE

  VerbCall{}    -> notimp
  BuiltinFunc{} -> notimp

  a `Plus`   b -> binary plus   a b
  a `Minus`  b -> binary minus  a b
  a `Times`  b -> binary times  a b
  a `Divide` b -> binary divide a b
  a `Remain` b -> binary remain a b
  a `Power`  b -> binary power  a b

  Negate a -> do a' <- compileExpr a
                 case a' of
                   (Int x) -> return $ Int (-x)
                   (Flt x) -> return $ Flt (-x)
                   _       -> raise E_TYPE

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

  Index expr index -> do
    expr'  <- compileExpr expr
    index' <- withIndexLength expr' $ compileExpr index
    case index' of
      Int i' -> let i = fromIntegral i' in
        case expr' of
          Lst v -> do checkLstRange v i
                      return $ v V.! pred i
          Str t -> do checkStrRange t i
                      return $ Str $ T.singleton $ t `T.index` pred i
          _     -> raise E_TYPE
      _      -> raise E_TYPE

  Range expr (start, end) -> do
    expr' <- compileExpr expr
    (low, high) <- withIndexLength expr' $ do
      start' <- compileExpr start
      end'   <- compileExpr end
      case start' of
        Int s -> case end' of
          Int e -> return (fromIntegral s, fromIntegral e)
          _     -> raise E_TYPE
        _     -> raise E_TYPE
    if low > high
      then case expr' of
        Lst{} -> return $ Lst V.empty
        Str{} -> return $ Str T.empty
        _     -> raise E_TYPE
      else let len = high - low + 1 in case expr' of
        Lst v -> do checkLstRange v low >> checkLstRange v high
                    return $ Lst $ V.slice (pred low) len v
        Str t -> do checkStrRange t low >> checkStrRange t high
                    return $ Str $ T.take len $ T.drop (pred low) t
        _ -> raise E_TYPE

  Length -> reader indexLength >>= fmap (Int . fromIntegral)

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
        withIndexLength expr =
          local $ \r -> r { indexLength = case expr of
                               Lst v -> return $ V.length v
                               Str t -> return $ T.length t
                               _     -> raise E_TYPE
                          }
        checkLstRange v i =
          when (i < 1 || i > V.length v)            $ raise E_RANGE
        checkStrRange t i =
          when (i < 1 || T.compareLength t i == LT) $ raise E_RANGE

assign :: Id -> Value -> MOO Value
assign var value = do
  modifyFrame $ \frame -> frame {
    variables = Map.insert (T.toCaseFold var) value (variables frame)
    }
  return value

scatterAssign :: [ScatItem] -> LstT -> MOO Value
scatterAssign items args =
  if nargs < nreqs || (not haveRest && nargs > ntarg) then raise E_ARGS
  else do walk items args (nargs - nreqs)
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
                            Just expr -> do expr' <- compileExpr expr
                                            void $ assign var expr'
                walk items args noptAvail
            ScatRest var -> do
              let (s, r) = V.splitAt nrest args
              assign var (Lst s)
              walk items r noptAvail
        walk [] _ _ = return ()

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
