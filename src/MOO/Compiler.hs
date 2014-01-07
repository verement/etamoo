
{-# LANGUAGE OverloadedStrings #-}

module MOO.Compiler ( compile, evaluate ) where

import Control.Monad (when, unless, void, liftM)
import Control.Monad.Cont (callCC)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (gets)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

import MOO.Types
import MOO.AST
import MOO.Task
import MOO.Builtins
import MOO.Object

compile :: Program -> MOO Value
compile (Program stmts) = callCC $ compileStatements stmts

compileStatements :: [Statement] -> (Value -> MOO Value) -> MOO Value
compileStatements (statement:rest) yield = catchDebug $ case statement of
  Expression lineNumber expr -> do
    setLineNumber lineNumber
    evaluate expr
    compile' rest

  If lineNumber cond (Then thens) elseIfs (Else elses) -> runTick >> do
    compileIf ((lineNumber, cond, thens) : map elseIf elseIfs) elses
    compile' rest

    where elseIf (ElseIf lineNumber cond thens) = (lineNumber, cond, thens)

          compileIf ((lineNumber,cond,thens):conds) elses = do
            setLineNumber lineNumber
            cond' <- truthOf `liftM` evaluate cond
            if cond' then compile' thens
              else compileIf conds elses
          compileIf [] elses = compile' elses

  ForList lineNumber var expr body -> do
    setLineNumber lineNumber
    expr' <- evaluate expr
    elts <- case expr' of
      Lst elts -> return $ V.toList elts
      _        -> raise E_TYPE

    callCC $ \break -> do
      pushLoopContext (Just var') (Continuation break)
      loop var' elts (compile' body)
    popContext

    compile' rest

    where var' = T.toCaseFold var
          loop var (elt:elts) body = runTick >> do
            storeVariable var elt
            callCC $ \continue -> do
              setLoopContinue (Continuation continue)
              body
            loop var elts body
          loop _ [] _ = return nothing

  ForRange lineNumber var (start, end) body -> do
    setLineNumber lineNumber
    start' <- evaluate start
    end'   <- evaluate end
    (ty, s, e) <- case (start', end') of
      (Int s, Int e) -> return (Int . fromIntegral, toInteger s, toInteger e)
      (Obj s, Obj e) -> return (Obj . fromIntegral, toInteger s, toInteger e)
      (_    , _    ) -> raise E_TYPE

    callCC $ \break -> do
      pushLoopContext (Just var') (Continuation break)
      loop var' ty s e (compile' body)
    popContext

    compile' rest

    where var' = T.toCaseFold var
          loop var ty i end body
            | i > end   = return nothing
            | otherwise = runTick >> do
              storeVariable var (ty i)
              callCC $ \continue -> do
                setLoopContinue (Continuation continue)
                body
              loop var ty (succ i) end body

  While lineNumber var expr body -> do
    callCC $ \break -> do
      pushLoopContext var' (Continuation break)
      loop lineNumber var' (evaluate expr) (compile' body)
      return nothing
    popContext

    compile' rest

    where var' = fmap T.toCaseFold var
          loop lineNumber var expr body = runTick >> do
            setLineNumber lineNumber
            expr' <- expr
            maybe return storeVariable var expr'
            when (truthOf expr') $ do
              callCC $ \continue -> do
                setLoopContinue (Continuation continue)
                body
              loop lineNumber var expr body

  Fork lineNumber var delay body -> runTick >> do
    setLineNumber lineNumber
    delay' <- evaluate delay
    usecs <- case delay' of
      Int secs
        | secs < 0  -> raise E_INVARG
        | otherwise -> return (fromIntegral secs * 1000000)
      Flt secs
        | secs < 0  -> raise E_INVARG
        | otherwise -> return (ceiling    $ secs * 1000000)
      _ -> raise E_TYPE

    world <- getWorld
    gen <- newRandomGen
    taskId <- liftSTM $ newTaskId world gen
    case var of
      Just ident -> void $ storeVariable ident (Int $ fromIntegral taskId)
      Nothing    -> return ()

    forkTask taskId usecs (compileStatements body return)

    compile' rest

  Break    name -> breakLoop    (fmap T.toCaseFold name)
  Continue name -> continueLoop (fmap T.toCaseFold name)

  Return _          Nothing     -> runTick >> yield nothing
  Return lineNumber (Just expr) -> runTick >> do
    setLineNumber lineNumber
    yield =<< evaluate expr

  TryExcept body excepts -> runTick >> do
    excepts' <- mapM compileExcepts excepts

    compile' body `catchException` dispatch excepts'
    compile' rest

    where compileExcepts (Except lineNumber var codes handler) = do
            codes' <- case codes of
              ANY        -> return Nothing
              Codes args -> setLineNumber lineNumber >> Just `liftM` expand args
            return (codes', var, compile' handler)

          dispatch ((codes, var, handler):next)
            except@(Exception code message value)
            | maybe True (code `elem`) codes = \(Stack errorFrames) -> do
              Stack currentFrames <- gets stack
              let traceback = formatFrames True $ take stackLen errorFrames
                  stackLen  = length errorFrames - length currentFrames + 1
                  errorInfo = fromList [code, Str message, value, traceback]
              maybe return storeVariable var errorInfo
              handler
            | otherwise = dispatch next except
          dispatch [] except = passException except

  TryFinally body (Finally finally) -> runTick >> do
    let finally' = compile' finally
    pushTryFinallyContext finally'

    compile' body `catchException` \except callStack -> do
      popContext
      finally'
      passException except callStack

    popContext
    finally'

    compile' rest

  where compile' ss = compileStatements ss yield

compileStatements [] _ = return nothing

catchDebug :: MOO Value -> MOO Value
catchDebug action =
  action `catchException` \except@(Exception code _ _) callStack -> do
    debug <- frame debugBit
    if debug then passException except callStack else return code

evaluate :: Expr -> MOO Value
evaluate (Literal value) = return value
evaluate expr@Variable{} = catchDebug $ fetch (lValue expr)
evaluate expr = runTick >>= \_ -> catchDebug $ case expr of
  List args -> fromList `liftM` expand args

  PropRef{} -> fetch (lValue expr)

  Assign what expr -> store (lValue what) =<< evaluate expr

  ScatterAssign items expr -> do
    expr' <- evaluate expr
    case expr' of
      Lst v -> scatterAssign items v
      _     -> raise E_TYPE

  VerbCall target vname args -> do
    target' <- evaluate target
    oid <- case target' of
      Obj oid -> return oid
      _       -> raise E_TYPE

    vname' <- evaluate vname
    name <- case vname' of
      Str name -> return name
      _        -> raise E_TYPE

    callVerb oid oid name =<< expand args

  BuiltinFunc func args -> callBuiltin (T.toCaseFold func) =<< expand args

  a `Plus`   b -> binary plus   a b
  a `Minus`  b -> binary minus  a b
  a `Times`  b -> binary times  a b
  a `Divide` b -> binary divide a b
  a `Remain` b -> binary remain a b
  a `Power`  b -> binary power  a b

  Negate a -> do a' <- evaluate a
                 case a' of
                   Int x -> return (Int $ negate x)
                   Flt x -> return (Flt $ negate x)
                   _     -> raise E_TYPE

  Conditional c x y -> do c' <- evaluate c
                          evaluate $ if truthOf c' then x else y

  x `And` y -> do x' <- evaluate x
                  if truthOf x' then evaluate y else return x'
  x `Or`  y -> do x' <- evaluate x
                  if truthOf x' then return x' else evaluate y

  Not x -> (truthValue . not . truthOf) `liftM` evaluate x

  x `Equal`        y -> equality   (==) x y
  x `NotEqual`     y -> equality   (/=) x y
  x `LessThan`     y -> comparison (<)  x y
  x `LessEqual`    y -> comparison (<=) x y
  x `GreaterThan`  y -> comparison (>)  x y
  x `GreaterEqual` y -> comparison (>=) x y

  Index{} -> fetch (lValue expr)
  Range{} -> fetch (lValue expr)

  Length -> liftM (Int . fromIntegral) =<< asks indexLength

  item `In` list -> do
    item' <- evaluate item
    list' <- evaluate list
    case list' of
      Lst v -> return $ Int $ maybe 0 (fromIntegral . succ) $
               V.elemIndex item' v
      _     -> raise E_TYPE

  Catch expr codes (Default dv) -> do
    codes' <- case codes of
      ANY        -> return Nothing
      Codes args -> Just `liftM` expand args
    evaluate expr `catchException` \except@(Exception code _ _) callStack ->
      if maybe True (code `elem`) codes'
        then maybe (return code) evaluate dv
        else passException except callStack

  where binary op a b = do
          a' <- evaluate a
          b' <- evaluate b
          a' `op` b'
        equality op = binary test
          where test a b = return $ truthValue (a `op` b)
        comparison op = binary test
          where test a b = do when (typeOf a /= typeOf b) $ raise E_TYPE
                              case a of
                                Lst{} -> raise E_TYPE
                                _     -> return $ truthValue (a `op` b)

fetchVariable :: Id -> MOO Value
fetchVariable var =
  maybe (raise E_VARNF) return . M.lookup var =<< frame variables

storeVariable :: Id -> Value -> MOO Value
storeVariable var value = do
  modifyFrame $ \frame ->
    frame { variables = M.insert var value (variables frame) }
  return value

fetchProperty :: (ObjT, StrT) -> MOO Value
fetchProperty (oid, name) = do
  obj <- getObject oid >>= maybe (raise E_INVIND) return
  maybe (search False obj) (return . ($ obj)) $ builtinProperty name'
  where name' = T.toCaseFold name

        search skipPermCheck obj = do
          prop <- getProperty obj name'
          unless (skipPermCheck || propertyPermR prop) $
            checkPermission (propertyOwner prop)
          case propertyValue prop of
            Just value -> return value
            Nothing    -> do
              parentObj <- maybe (return Nothing) getObject (objectParent obj)
              maybe (error $ "No inherited value for property " ++
                     T.unpack name) (search True) parentObj

storeProperty :: (ObjT, StrT) -> Value -> MOO Value
storeProperty (oid, name) value = do
  obj <- getObject oid >>= maybe (raise E_INVIND) return
  if isBuiltinProperty name'
    then setBuiltinProperty (oid, obj) name' value
    else modifyProperty obj name' $ \prop -> do
      unless (propertyPermW prop) $ checkPermission (propertyOwner prop)
      return prop { propertyValue = Just value }
  return value
  where name' = T.toCaseFold name

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
        fetch = fetchVariable var'
        store = storeVariable var'

        change = do
          value <- fetch
          return (value, store)

lValue (PropRef objExpr nameExpr) = LValue fetch store change
  where fetch = getRefs >>= fetchProperty
        store value = getRefs >>= flip storeProperty value

        change = do
          value <- fetch
          return (value, store)
          -- XXX improve this to avoid redundant checks?

        getRefs = do
          objRef  <- evaluate objExpr
          nameRef <- evaluate nameExpr
          case (objRef, nameRef) of
            (Obj oid, Str name) -> return (oid, name)
            _                   -> raise E_TYPE

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
          index' <- checkIndex =<< withIndexLength value (evaluate index)
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
          start' <- checkIndex =<< evaluate start
          end'   <- checkIndex =<< evaluate end
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
  where fetch = evaluate expr
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
                            Just expr -> void $ assign var =<< evaluate expr
                walk items args noptAvail
            ScatRest var -> do
              let (s, r) = V.splitAt nrest args
              assign var (Lst s)
              walk items r noptAvail
        walk [] _ _ = return ()

        assign var = storeVariable (T.toCaseFold var)

expand :: [Arg] -> MOO [Value]
expand (a:as) = case a of
  ArgNormal expr -> do a' <- evaluate expr
                       (a' :) `liftM` expand as
  ArgSplice expr -> do a' <- evaluate expr
                       case a' of
                         Lst v -> (V.toList v ++) `liftM` expand as
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
        roundZero :: FltT -> Integer
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
