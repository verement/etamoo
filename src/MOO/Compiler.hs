
-- | Compiling abstract syntax trees into 'MOO' computations
module MOO.Compiler ( compile, evaluate ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, unless, void, liftM, (<=<))
import Control.Monad.Cont (callCC)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (gets)
import Data.Monoid ((<>))

import qualified Data.Map as M
import qualified Data.Vector as V

import MOO.AST
import MOO.Builtins
import MOO.Object
import MOO.Task
import MOO.Types

import qualified MOO.String as Str

-- | Compile a complete MOO program into a computation in the 'MOO' monad that
-- returns whatever the MOO program returns.
compile :: Program -> MOO Value
compile (Program stmts) = callCC $ compileStatements stmts

compileStatements :: [Statement] -> (Value -> MOO Value) -> MOO Value
compileStatements (statement:rest) yield = case statement of
  Expression lineNo expr ->
    setLineNumber lineNo >> evaluate expr >> compile' rest

  If lineNo cond (Then thens) elseIfs (Else elses) -> runTick >> do
    compileIf ((lineNo, cond, thens) : map elseIf elseIfs) elses
    compile' rest

    where elseIf :: ElseIf -> (LineNo, Expr, [Statement])
          elseIf (ElseIf lineNo cond thens) = (lineNo, cond, thens)

          compileIf :: [(LineNo, Expr, [Statement])] -> [Statement] -> MOO Value
          compileIf ((lineNo,cond,thens):conds) elses = do
            setLineNumber lineNo
            cond' <- evaluate cond
            if truthOf cond' then compile' thens
              else compileIf conds elses
          compileIf [] elses = compile' elses

  ForList lineNo var expr body -> do
    handleDebug $ do
      setLineNumber lineNo
      elts <- getList =<< evaluate expr

      callCC $ \break -> do
        pushLoopContext (Just var) (Continuation break)
        loop var elts (compile' body)
      popContext
      return zero

    compile' rest

    where loop :: Id -> [Value] -> MOO a -> MOO ()
          loop var (elt:elts) body = runTick >> do
            storeVariable var elt
            callCC $ \continue -> do
              setLoopContinue (Continuation continue)
              void body
            loop var elts body
          loop _ [] _ = return ()

  ForRange lineNo var (start, end) body -> do
    handleDebug $ do
      setLineNumber lineNo
      start' <- evaluate start
      end'   <- evaluate end
      (ty, s, e) <- case (start', end') of
        (Int s, Int e) -> return (Int . fromInteger, toInteger s, toInteger e)
        (Obj s, Obj e) -> return (Obj . fromInteger, toInteger s, toInteger e)
        (_    , _    ) -> raise E_TYPE

      callCC $ \break -> do
        pushLoopContext (Just var) (Continuation break)
        loop var ty s e (compile' body)
      popContext
      return zero

    compile' rest

    where loop :: Id -> (Integer -> Value) -> Integer -> Integer -> MOO a ->
                  MOO ()
          loop var ty i end body
            | i > end   = return ()
            | otherwise = runTick >> do
              storeVariable var (ty i)
              callCC $ \continue -> do
                setLoopContinue (Continuation continue)
                void body
              loop var ty (succ i) end body

  While lineNo var expr body -> do
    callCC $ \break -> do
      pushLoopContext var (Continuation break)
      loop lineNo var (evaluate expr) (compile' body)
    popContext

    compile' rest

    where loop :: LineNo -> Maybe Id -> MOO Value -> MOO a -> MOO ()
          loop lineNo var expr body = runTick >> do
            setLineNumber lineNo
            expr' <- expr
            maybe return storeVariable var expr'
            when (truthOf expr') $ do
              callCC $ \continue -> do
                setLoopContinue (Continuation continue)
                void body
              loop lineNo var expr body

  Fork lineNo var delay body -> runTick >> do
    handleDebug $ do
      setLineNumber lineNo
      delay' <- evaluate delay
      usecs <- case delay' of
        Int secs
          | secs < 0  -> raise E_INVARG
          | otherwise -> return (fromIntegral secs * 1000000)
        Flt secs
          | secs < 0  -> raise E_INVARG
          | otherwise -> return (ceiling    $ secs * 1000000)
        _ -> raise E_TYPE

      checkQueuedTaskLimit

      world <- getWorld
      gen <- newRandomGen
      let taskId = newTaskId world gen
      maybe return storeVariable var (Int $ fromIntegral taskId)

      forkTask taskId usecs (compileStatements body return)
      return zero

    compile' rest

  Break    name -> breakLoop    name
  Continue name -> continueLoop name

  Return _       Nothing    -> runTick >> yield zero
  Return lineNo (Just expr) -> runTick >> do
    setLineNumber lineNo
    yield =<< evaluate expr

  TryExcept body excepts -> runTick >> do
    excepts' <- mapM compileExcept excepts

    compile' body `catchException` dispatch excepts'
    compile' rest

    where compileExcept :: Except -> MOO (Maybe [Value], Maybe Id, MOO Value)
          compileExcept (Except lineNo var codes handler) = do
            codes' <- case codes of
              ANY        -> return Nothing
              Codes args -> setLineNumber lineNo >> Just <$> expand args
            return (codes', var, compile' handler)

          dispatch :: [(Maybe [Value], Maybe Id, MOO Value)] -> Exception ->
                      MOO Value
          dispatch ((codes, var, handler):next) except@Exception {
              exceptionCode      = code
            , exceptionMessage   = message
            , exceptionValue     = value
            , exceptionCallStack = Stack errorFrames
            }
            | maybe True (code `elem`) codes = do
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

    compile' body `catchException` \except -> do
      popContext
      finally'
      passException except

    popContext
    finally'

    compile' rest

  where compile' :: [Statement] -> MOO Value
        compile' ss = compileStatements ss yield

compileStatements [] _ = return zero

-- | Compile a MOO expression into a computation in the 'MOO' monad. If a MOO
-- exception is raised and the current verb frame's debug bit is not set,
-- return the error code as a MOO value rather than propagating the exception.
evaluate :: Expr -> MOO Value
evaluate (Literal value) = return value
evaluate expr@Variable{} = handleDebug $ fetch (lValue expr)
evaluate expr = runTick >>= \_ -> handleDebug $ case expr of
  List args -> fromList <$> expand args

  PropertyRef{} -> fetch (lValue expr)

  Assign what expr -> evaluate expr >>= store (lValue what)

  Scatter items expr -> evaluate expr >>= usingList (scatterAssign items)

  VerbCall target vname args -> do
    target' <- evaluate target
    vname'  <- evaluate vname
    args'   <- expand args

    (oid, name) <- case (target', vname') of
      (Obj oid, Str name) -> return (oid, name)
      (_      , _       ) -> raise E_TYPE

    callVerb oid oid name args'

  BuiltinFunc func args -> expand args >>= callBuiltin func

  a `Plus`   b -> binary plus   a b
  a `Minus`  b -> binary minus  a b
  a `Times`  b -> binary times  a b
  a `Divide` b -> binary divide a b
  a `Remain` b -> binary remain a b
  a `Power`  b -> binary power  a b

  Negate x -> evaluate x >>= \x' -> case x' of
    Int n -> return (Int $ negate n)
    Flt n -> return (Flt $ negate n)
    _     -> raise E_TYPE

  Conditional cond x y ->
    evaluate cond >>= \cond' -> evaluate $ if truthOf cond' then x else y

  x `And` y -> evaluate x >>= \v -> if truthOf v then evaluate y else return   v
  x `Or`  y -> evaluate x >>= \v -> if truthOf v then return   v else evaluate y

  Not x -> truthValue . not . truthOf <$> evaluate x

  x `CompareEQ` y -> equality   (==) x y
  x `CompareNE` y -> equality   (/=) x y
  x `CompareLT` y -> comparison (<)  x y
  x `CompareLE` y -> comparison (<=) x y
  x `CompareGT` y -> comparison (>)  x y
  x `CompareGE` y -> comparison (>=) x y

  Index{} -> fetch (lValue expr)
  Range{} -> fetch (lValue expr)

  Length -> liftM (Int . fromIntegral) =<< asks indexLength

  item `In` list -> do
    elt <- evaluate item
    evaluate list >>= usingList
      (return . Int . maybe 0 (fromIntegral . succ) . V.elemIndex elt)

  Catch expr codes (Default dv) -> do
    codes' <- case codes of
      ANY        -> return Nothing
      Codes args -> Just <$> expand args
    evaluate expr `catchException` \except@Exception { exceptionCode = code } ->
      if maybe True (code `elem`) codes'
      then maybe (return code) evaluate dv
      else passException except

  where binary :: (Value -> Value -> MOO Value) -> Expr -> Expr -> MOO Value
        binary op x y = evaluate x >>= \x' -> evaluate y >>= \y' -> x' `op` y'
        -- binary op x y = join $ op <$> evaluate x <*> evaluate y

        equality :: (Value -> Value -> Bool) -> Expr -> Expr -> MOO Value
        equality op = binary test
          where test a b = return $ truthValue (a `op` b)

        comparison :: (Value -> Value -> Bool) -> Expr -> Expr -> MOO Value
        comparison op = binary test
          where test x y | comparable x y = return $ truthValue (x `op` y)
                         | otherwise      = raise E_TYPE

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
  obj <- maybe (raise E_INVIND) return =<< getObject oid
  maybe (search False obj) (return . ($ obj)) $ builtinProperty name

  where search :: Bool -> Object -> MOO Value
        search skipPermCheck obj = do
          prop <- getProperty obj name
          unless (skipPermCheck || propertyPermR prop) $
            checkPermission (propertyOwner prop)
          case propertyValue prop of
            Just value -> return value
            Nothing    -> do
              parentObj <- maybe (return Nothing) getObject (objectParent obj)
              maybe (error $ "No inherited value for property " ++
                     Str.toString name) (search True) parentObj

storeProperty :: (ObjT, StrT) -> Value -> MOO Value
storeProperty (oid, name) value = do
  obj <- maybe (raise E_INVIND) return =<< getObject oid
  if isBuiltinProperty name
    then setBuiltinProperty (oid, obj) name value
    else modifyProperty obj name $ \prop -> do
      unless (propertyPermW prop) $ checkPermission (propertyOwner prop)
      return prop { propertyValue = Just value }
  return value

withIndexLength :: Value -> MOO a -> MOO a
withIndexLength expr =
  local $ \env -> env { indexLength = case expr of
                           Lst v -> return (V.length v)
                           Str t -> return (Str.length t)
                           _     -> raise E_TYPE
                      }

usingList :: (LstT -> MOO a) -> Value -> MOO a
usingList f (Lst v) = f v
usingList _  _      = raise E_TYPE

getList :: Value -> MOO [Value]
getList = usingList (return . V.toList)

getIndex :: Value -> MOO Int
getIndex (Int i) = return (fromIntegral i)
getIndex _       = raise E_TYPE

checkLstRange :: LstT -> Int -> MOO ()
checkLstRange v i = when (i < 1 || i > V.length v) $ raise E_RANGE

checkStrRange :: StrT -> Int -> MOO ()
checkStrRange t i = when (i < 1 || t `Str.compareLength` i == LT) $
                    raise E_RANGE

data LValue = LValue {
    fetch  :: MOO Value
  , store  :: Value -> MOO Value
  , change :: MOO (Value, Value -> MOO Value)
  }

lValue :: Expr -> LValue

lValue (Variable var) = LValue fetch store change
  where fetch  = fetchVariable var
        store  = storeVariable var
        change = fetch >>= \value -> return (value, store)

lValue (PropertyRef objExpr nameExpr) = LValue fetch store change
  where fetch       = getRefs >>= fetchProperty
        store value = getRefs >>= flip storeProperty value

        change = do
          refs <- getRefs
          value <- fetchProperty refs
          return (value, storeProperty refs)

        getRefs = do
          objRef  <- evaluate objExpr
          nameRef <- evaluate nameExpr
          case (objRef, nameRef) of
            (Obj oid, Str name) -> return (oid, name)
            _                   -> raise E_TYPE

lValue (expr `Index` index) = LValue fetchIndex storeIndex changeIndex
  where fetchIndex = fst <$> changeIndex

        storeIndex newValue = do
          (_, change) <- changeIndex
          change newValue
          return newValue

        changeIndex = do
          (value, changeExpr) <- change (lValue expr)
          index' <- getIndex =<< withIndexLength value (evaluate index)
          value' <- case value of
            Lst v -> checkLstRange v index' >> return (v V.! (index' - 1))
            Str t -> checkStrRange t index' >>
                     return (Str $ Str.singleton $ t `Str.index` (index' - 1))
            _     -> raise E_TYPE
          return (value', changeValue value index' changeExpr)

        changeValue :: Value -> Int -> (Value -> MOO a) -> Value -> MOO a
        changeValue (Lst v) index changeExpr newValue =
          changeExpr $ Lst $ listSet v (index - 1) newValue
        changeValue (Str t) index changeExpr (Str c) = do
          when (c `Str.compareLength` 1 /= EQ) $ raise E_INVARG
          let (s, r) = Str.splitAt (index - 1) t
          changeExpr $ Str $ Str.concat [s, c, Str.tail r]
        changeValue _ _ _ _ = raise E_TYPE

lValue (expr `Range` (start, end)) = LValue fetchRange storeRange changeRange
  where fetchRange = do
          value <- fetch (lValue expr)
          (start', end') <- getIndices value
          if start' > end'
            then case value of
              Lst{} -> return emptyList
              Str{} -> return emptyString
              _     -> raise E_TYPE
            else let len = end' - start' + 1 in case value of
              Lst v -> do checkLstRange v start' >> checkLstRange v end'
                          return $ Lst $ V.slice (start' - 1) len v
              Str t -> do checkStrRange t start' >> checkStrRange t end'
                          return $ Str $ Str.take len $ Str.drop (start' - 1) t
              _     -> raise E_TYPE

        storeRange newValue = do
          (value, changeExpr) <- change (lValue expr)
          startEnd <- getIndices value
          changeValue value startEnd changeExpr newValue
          return newValue

        changeRange = error "Illegal Range as lvalue subexpression"

        getIndices :: Value -> MOO (Int, Int)
        getIndices value = withIndexLength value $
                           (,) <$> (evaluate start >>= getIndex)
                               <*> (evaluate end   >>= getIndex)

        changeValue :: Value -> (Int, Int) -> (Value -> MOO a) -> Value -> MOO a
        changeValue (Lst v) (start, end) changeExpr (Lst r) = do
          let len = V.length v
          when (end < 0 || start > len + 1) $ raise E_RANGE
          let pre  = sublist v 1 (start - 1)
              post = sublist v (end + 1) len
              sublist v s e
                | e < s     = V.empty
                | otherwise = V.slice (s - 1) (e - s + 1) v
          changeExpr $ Lst $ V.concat [pre, r, post]
        changeValue (Str t) (start, end) changeExpr (Str r) = do
          when (end < 0 || t `Str.compareLength` (start - 1) == LT) $
            raise E_RANGE
          let pre  = substr t 1 (start - 1)
              post = substr t (end + 1) (Str.length t)
              substr t s e
                | e < s     = Str.empty
                | otherwise = Str.take (e - s + 1) $ Str.drop (s - 1) t
          changeExpr $ Str $ Str.concat [pre, r, post]
        changeValue _ _ _ _ = raise E_TYPE

lValue expr = LValue fetch store change
  where fetch   = evaluate expr
        store _ = error "Unmodifiable LValue"
        change  = fetch >>= \value -> return (value, store)

scatterAssign :: [ScatterItem] -> LstT -> MOO Value
scatterAssign items args = do
  when (nargs < nreqs || (not haveRest && nargs > ntarg)) $ raise E_ARGS
  walk items args (nargs - nreqs)
  return (Lst args)

  where nargs = V.length args
        nreqs = count required items
        nopts = count optional items
        ntarg = nreqs + nopts
        nrest = if haveRest && nargs >= ntarg then nargs - ntarg else 0

        count p  = length . filter p
        haveRest = any rest items

        required, optional, rest :: ScatterItem -> Bool
        required ScatRequired{} = True
        required _              = False
        optional ScatOptional{} = True
        optional _              = False
        rest     ScatRest{}     = True
        rest     _              = False

        walk :: [ScatterItem] -> LstT -> Int -> MOO ()
        walk (item:items) args noptAvail = case item of
          ScatRequired var -> do
            storeVariable var (V.head args)
            walk items (V.tail args) noptAvail
          ScatOptional var opt
            | noptAvail > 0 -> do
                storeVariable var (V.head args)
                walk items (V.tail args) (noptAvail - 1)
            | otherwise     -> do
                maybe (return zero) (storeVariable var <=< evaluate) opt
                walk items args noptAvail
          ScatRest var -> do
            let (s, r) = V.splitAt nrest args
            storeVariable var (Lst s)
            walk items r noptAvail
        walk [] _ _ = return ()

expand :: [Argument] -> MOO [Value]
expand (x:xs) = case x of
  ArgNormal expr -> (:)  <$>  evaluate expr              <*> expand xs
  ArgSplice expr -> (++) <$> (evaluate expr >>= getList) <*> expand xs
expand [] = return []

plus :: Value -> Value -> MOO Value
Int a `plus` Int b = return $ Int (a +  b)
Flt a `plus` Flt b = checkFloat   (a +  b)
Str a `plus` Str b = return $ Str (a <> b)
_     `plus` _     = raise E_TYPE

minus :: Value -> Value -> MOO Value
Int a `minus` Int b = return $ Int (a - b)
Flt a `minus` Flt b = checkFloat   (a - b)
_     `minus` _     = raise E_TYPE

times :: Value -> Value -> MOO Value
Int a `times` Int b = return $ Int (a * b)
Flt a `times` Flt b = checkFloat   (a * b)
_     `times` _     = raise E_TYPE

divide :: Value -> Value -> MOO Value
Int _ `divide` Int   0 = raise E_DIV
Int a `divide` Int (-1)
  | a == minBound      = return $ Int  a  -- avoid arithmetic overflow exception
Int a `divide` Int   b = return $ Int (a `quot` b)
Flt _ `divide` Flt   0 = raise E_DIV
Flt a `divide` Flt   b = checkFloat   (a / b)
_     `divide` _       = raise E_TYPE

remain :: Value -> Value -> MOO Value
Int _ `remain` Int 0 = raise E_DIV
Int a `remain` Int b = return $ Int (a `rem`  b)
Flt _ `remain` Flt 0 = raise E_DIV
Flt a `remain` Flt b = checkFloat   (a `fmod` b)
_     `remain` _     = raise E_TYPE

fmod :: FltT -> FltT -> FltT
x `fmod` y = x - y * fromInteger (roundZero $ x / y)
  where roundZero :: FltT -> Integer
        roundZero q | q > 0     = floor   q
                    | q < 0     = ceiling q
                    | otherwise = round   q

power :: Value -> Value -> MOO Value
Flt   a  `power` Flt b             = checkFloat   (a ** b)
Flt   a  `power` Int b             = checkFloat   (a ^^ b)
Int   a  `power` Int b | b >= 0    = return $ Int (a ^  b)
--                     | b <  0 ...
Int   0  `power` Int _             = raise E_DIV
Int   1  `power` Int _             = return $ Int   1
Int (-1) `power` Int b | even b    = return $ Int   1
                       | otherwise = return $ Int (-1)
Int   _  `power` Int _             = return $ Int   0
_        `power` _                 = raise E_TYPE
