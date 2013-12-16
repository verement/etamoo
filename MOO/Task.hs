
{-# LANGUAGE OverloadedStrings #-}

module MOO.Task ( MOO
                , Task ( .. )
                , TaskDisposition ( .. )
                , Resume ( .. )
                , DelayedIO ( .. )
                , Environment ( .. )
                , TaskState ( .. )
                , CallStack ( .. )
                , Continuation ( .. )
                , StackFrame ( .. )
                , Exception ( .. )
                , initTask
                , runTask
                , interrupt
                , liftSTM
                , initEnvironment
                , initState
                , getDatabase
                , putDatabase
                , getObject
                , getProperty
                , getVerb
                , modifyProperty
                , modifyVerb
                , setBuiltinProperty
                , reader
                , local
                , initFrame
                , formatStack
                , pushFrame
                , popFrame
                , frame
                , caller
                , modifyFrame
                , setLineNumber
                , pushTryFinallyContext
                , pushLoopContext
                , setLoopContinue
                , popContext
                , breakLoop
                , continueLoop
                , catchException
                , passException
                , raiseException
                , notyet
                , raise
                , checkFloat
                , checkProgrammer
                , checkWizard
                , checkPermission
                , checkValid
                , binaryString
                , random
                , delayIO
                , runContT
                , evalStateT
                , runReaderT
                ) where

import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow (first)
import Control.Concurrent.STM
import System.Random hiding (random)
import System.Time
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Monoid
import Data.Maybe (isNothing)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.ByteString as BS

import MOO.Types
import {-# SOURCE #-} MOO.Database
import MOO.Object
import MOO.Verb

type MOO = ReaderT Environment
           (ContT TaskDisposition
            (StateT TaskState STM))

liftSTM :: STM a -> MOO a
liftSTM = lift . lift . lift

data Task = Task {
    taskId          :: IntT
  , taskDatabase    :: TVar Database
  , taskComputation :: MOO Value
  , taskState       :: TaskState
}

initTask :: TVar Database -> MOO Value -> IO Task
initTask db comp = do
  taskId <- randomRIO (1, maxBound)
  gen <- getStdGen
  return Task {
      taskId          = taskId
    , taskDatabase    = db
    , taskComputation = comp
    , taskState       = initState gen
  }

data TaskDisposition = Complete Value
                     | Abort Exception CallStack
                     | Suspend (Maybe IntT) Resume
                     | Read Resume

newtype Resume = Resume (Value -> MOO Value)

runTask :: Task -> IO (TaskDisposition, Task)
runTask task = do
  env <- initEnvironment task
  let comp   = taskComputation task
      comp'  = callCC $ \k ->
        fmap Complete $ local (\r -> r { interruptHandler = k }) comp
      state  = taskState task
      contM  = runReaderT comp' env
      stateM = runContT contM return
      stmM   = runStateT stateM state
  (result, state') <- atomically stmM
  runDelayed $ delayedIO state'
  return (result, task { taskState = state' { delayedIO = mempty }})

interrupt :: TaskDisposition -> MOO a
interrupt disp = do
  asks interruptHandler >>= ($ disp)
  error "Returned from interrupt handler"

newtype DelayedIO = DelayedIO { runDelayed :: IO () }

instance Monoid DelayedIO where
  mempty = DelayedIO $ return ()
  (DelayedIO a) `mappend` (DelayedIO b) = DelayedIO (a >> b)

delayIO :: IO () -> MOO ()
delayIO io = do
  existing <- gets delayedIO
  modify $ \st -> st { delayedIO = existing `mappend` DelayedIO io }

data Environment = Env {
    task             :: Task
  , interruptHandler :: TaskDisposition -> MOO TaskDisposition
  , startTime        :: ClockTime
  , exceptionHandler :: ExceptionHandler
  , indexLength      :: MOO Int
}

initEnvironment :: Task -> IO Environment
initEnvironment task = do
  startTime <- getClockTime
  return Env {
      task             = task
    , interruptHandler = error "Undefined interrupt handler"
    , startTime        = startTime
    , exceptionHandler = Handler $ \e cs -> interrupt (Abort e cs)
    , indexLength      = error "Invalid index context"
  }

data TaskState = State {
    stack     :: CallStack
  , randomGen :: StdGen
  , delayedIO :: DelayedIO
}

initState :: StdGen -> TaskState
initState gen = State {
    stack     = Stack []
  , randomGen = gen
  , delayedIO = mempty
}

getDatabase :: MOO Database
getDatabase = liftSTM . readTVar . taskDatabase =<< asks task

putDatabase :: Database -> MOO ()
putDatabase db = do
  dbTVar <- fmap taskDatabase $ asks task
  liftSTM $ writeTVar dbTVar db

getObject :: ObjId -> MOO (Maybe Object)
getObject oid = liftSTM . dbObject oid =<< getDatabase

getProperty :: Object -> StrT -> MOO Property
getProperty obj name = do
  maybeProp <- liftSTM $ lookupProperty obj (T.toCaseFold name)
  maybe (raise E_PROPNF) return maybeProp

getVerb :: Object -> Value -> MOO Verb
getVerb obj desc@Str{} = do
  maybeVerb <- liftSTM $ lookupVerb obj desc
  maybe (raise E_VERBNF) return maybeVerb
getVerb obj desc@(Int index)
  | index < 1 = raise E_INVARG
  | otherwise = do
    maybeVerb <- liftSTM $ lookupVerb obj desc
    maybe (raise E_VERBNF) return maybeVerb
getVerb _ _ = raise E_TYPE

modifyProperty :: Object -> StrT -> (Property -> MOO Property) -> MOO ()
modifyProperty obj name f =
  case lookupPropertyRef obj (T.toCaseFold name) of
    Nothing       -> raise E_PROPNF
    Just propTVar -> do
      prop  <- liftSTM $ readTVar propTVar
      prop' <- f prop
      liftSTM $ writeTVar propTVar prop'

modifyVerb :: (ObjId, Object) -> Value -> (Verb -> MOO Verb) -> MOO ()
modifyVerb (oid, obj) desc f =
  case lookupVerbRef obj desc of
    Nothing                -> raise E_VERBNF
    Just (index, verbTVar) -> do
      verb  <- liftSTM $ readTVar verbTVar
      verb' <- f verb
      liftSTM $ writeTVar verbTVar verb'
      let names  = T.toCaseFold $ verbNames verb
          names' = T.toCaseFold $ verbNames verb'
      when (names /= names') $ do
        db <- getDatabase
        liftSTM $ modifyObject oid db $ \obj ->
          return $ replaceVerb obj index verb'

setBuiltinProperty :: (ObjId, Object) -> StrT -> Value -> MOO ()
setBuiltinProperty (oid, obj) "name" (Str name) = do
  if objectIsPlayer obj
    then checkWizard
    else checkPermission (objectOwner obj)
  db <- getDatabase
  liftSTM $ modifyObject oid db $ \obj -> return obj { objectName = name }
setBuiltinProperty (oid, _) "owner" (Obj owner) = do
  checkWizard
  db <- getDatabase
  liftSTM $ modifyObject oid db $ \obj -> return obj { objectOwner = owner }
setBuiltinProperty _ "location" (Obj _) = raise E_PERM
setBuiltinProperty _ "contents" (Lst _) = raise E_PERM
setBuiltinProperty (oid, _) "programmer" bit = do
  checkWizard
  db <- getDatabase
  liftSTM $ modifyObject oid db $ \obj ->
    return obj { objectProgrammer = truthOf bit }
setBuiltinProperty (oid, _) "wizard" bit = do
  checkWizard
  db <- getDatabase
  liftSTM $ modifyObject oid db $ \obj ->
    return obj { objectWizard = truthOf bit }
setBuiltinProperty (oid, obj) "r" bit = do
  checkPermission (objectOwner obj)
  db <- getDatabase
  liftSTM $ modifyObject oid db $ \obj ->
    return obj { objectPermR = truthOf bit }
setBuiltinProperty (oid, obj) "w" bit = do
  checkPermission (objectOwner obj)
  db <- getDatabase
  liftSTM $ modifyObject oid db $ \obj ->
    return obj { objectPermW = truthOf bit }
setBuiltinProperty (oid, obj) "f" bit = do
  checkPermission (objectOwner obj)
  db <- getDatabase
  liftSTM $ modifyObject oid db $ \obj ->
    return obj { objectPermF = truthOf bit }
setBuiltinProperty _ _ _ = raise E_TYPE

newtype CallStack = Stack [StackFrame]
                  deriving Show

newtype Continuation = Continuation (Value -> MOO Value)

instance Show Continuation where
  show _ = "<Continuation>"

data Context =
  Loop {
    loopName     :: Maybe Id
  , loopBreak    :: Continuation
  , loopContinue :: Continuation
  } |
  TryFinally {
    finally      :: MOO Value
  }

instance Show Context where
  show Loop { loopName = Nothing   } = "<Loop>"
  show Loop { loopName = Just name } = "<Loop " ++ show name ++ ">"

  show TryFinally{} = "<TryFinally>"

data StackFrame = Frame {
    continuation  :: Continuation
  , contextStack  :: [Context]
  , variables     :: Map Id Value
  , debugBit      :: Bool
  , permissions   :: ObjId

  , verbName      :: StrT
  , verbLocation  :: ObjId
  , initialThis   :: ObjId
  , initialPlayer :: ObjId

  , lineNumber    :: IntT
} deriving Show

initFrame :: Bool -> ObjId -> StackFrame
initFrame debug programmer = Frame {
    continuation  = Continuation return
  , contextStack  = []
  , variables     = mkInitVars
  , debugBit      = debug
  , permissions   = programmer

  , verbName      = ""
  , verbLocation  = -1
  , initialThis   = -1
  , initialPlayer = -1

  , lineNumber    = 0
}

formatStack :: Bool -> CallStack -> Value
formatStack includeLineNumbers (Stack stack) =
  Lst $ V.fromList $ map formatFrame stack
  where formatFrame frame = Lst $ V.fromList $
                              Obj (initialThis   frame)
                            : Str (verbName      frame)
                            : Obj (permissions   frame)
                            : Obj (verbLocation  frame)
                            : Obj (initialPlayer frame)
                            : [Int $ lineNumber frame | includeLineNumbers]

pushFrame :: StackFrame -> MOO ()
pushFrame frame = modify $ \st@State { stack = Stack frames } ->
  st { stack = Stack (frame : frames) }

popFrame :: Value -> MOO Value
popFrame value = do
  unwindContexts (const False)
  (Continuation k) <- frame continuation
  modify $ \st@State { stack = Stack (_:frames) } -> st { stack = Stack frames }
  k value

currentFrame :: CallStack -> StackFrame
currentFrame (Stack (frame:_)) = frame
currentFrame (Stack [])        = error "Empty call stack"

previousFrame :: CallStack -> Maybe StackFrame
previousFrame (Stack (_:frame:_)) = Just frame
previousFrame (Stack [_])         = Nothing
previousFrame (Stack [])          = error "Empty call stack"

frame :: (StackFrame -> a) -> MOO a
frame f = gets (f . currentFrame . stack)

caller :: (StackFrame -> a) -> MOO (Maybe a)
caller f = gets (fmap f . previousFrame . stack)

modifyFrame :: (StackFrame -> StackFrame) -> MOO ()
modifyFrame f = modify $ \st@State { stack = Stack (frame:stack) } ->
  st { stack = Stack (f frame : stack) }

setLineNumber :: Int -> MOO ()
setLineNumber lineNumber = modifyFrame $ \frame ->
  frame { lineNumber = fromIntegral lineNumber }

pushContext :: Context -> MOO ()
pushContext context = modifyFrame $ \frame ->
  frame { contextStack = context : contextStack frame }

pushTryFinallyContext :: MOO Value -> MOO ()
pushTryFinallyContext finally =
  pushContext TryFinally { finally = finally }

pushLoopContext :: Maybe Id -> Continuation -> MOO ()
pushLoopContext name break =
  pushContext Loop {
      loopName     = name
    , loopBreak    = break
    , loopContinue = undefined
  }

setLoopContinue :: Continuation -> MOO ()
setLoopContinue continue =
  modifyFrame $ \frame@Frame { contextStack = loop:loops } ->
    frame { contextStack = loop { loopContinue = continue } : loops }

popContext :: MOO ()
popContext = modifyFrame $ \frame@Frame { contextStack = _:contexts } ->
  frame { contextStack = contexts }

unwindContexts :: (Context -> Bool) -> MOO [Context]
unwindContexts p = do
  stack <- unwind =<< frame contextStack
  modifyFrame $ \frame -> frame { contextStack = stack }
  return stack
  where unwind stack@(this:next) =
          if p this
          then return stack
          else do
            case this of
              TryFinally { finally = finally } -> do
                modifyFrame $ \frame -> frame { contextStack = next }
                finally
              _ -> return nothing
            unwind next
        unwind [] = return []

unwindLoopContext :: Maybe Id -> MOO Context
unwindLoopContext maybeName = do
  loop:_ <- unwindContexts testContext
  return loop
  where testContext Loop { loopName = name } =
          isNothing maybeName || maybeName == name
        testContext _ = False

breakLoop :: Maybe Id -> MOO Value
breakLoop maybeName = do
  Loop { loopBreak = Continuation break } <- unwindLoopContext maybeName
  break nothing

continueLoop :: Maybe Id -> MOO Value
continueLoop maybeName = do
  Loop { loopContinue = Continuation continue } <- unwindLoopContext maybeName
  continue nothing

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

newtype ExceptionHandler = Handler (Exception -> CallStack -> MOO Value)

instance Show ExceptionHandler where
  show _ = "<ExceptionHandler>"

data Exception = Exception Code Message Value
type Code = Value
type Message = StrT

catchException :: MOO a -> (Exception -> CallStack -> MOO a) -> MOO a
catchException action handler = callCC $ \k -> local (mkHandler k) action
  where mkHandler k env = env { exceptionHandler = Handler $ \e cs ->
                                 local (const env) $ handler e cs >>= k }

passException :: Exception -> CallStack -> MOO a
passException except callStack = do
  Handler handler <- asks exceptionHandler
  handler except callStack
  error "Returned from exception handler"

raiseException :: Exception -> MOO a
raiseException except = passException except =<< gets stack

notyet :: MOO a
notyet = raiseException $
         Exception (Err E_INVARG) "Not yet implemented" nothing

raise :: Error -> MOO a
raise err = raiseException $ Exception (Err err) (error2text err) nothing

checkFloat :: FltT -> MOO Value
checkFloat flt
  | isInfinite flt = raise E_FLOAT
  | isNaN      flt = raise E_INVARG
  | otherwise      = return (Flt flt)

checkProgrammer' :: ObjId -> MOO ()
checkProgrammer' perm = do
  programmer <- fmap (maybe False objectProgrammer) $ getObject perm
  unless programmer $ raise E_PERM

checkProgrammer :: MOO ()
checkProgrammer = checkProgrammer' =<< frame permissions

checkWizard' :: ObjId -> MOO ()
checkWizard' perm = do
  wizard <- fmap (maybe False objectWizard) $ getObject perm
  unless wizard $ raise E_PERM

checkWizard :: MOO ()
checkWizard = checkWizard' =<< frame permissions

checkPermission :: ObjId -> MOO ()
checkPermission who = do
  perm <- frame permissions
  unless (perm == who) $ checkWizard' perm

checkValid :: ObjId -> MOO Object
checkValid oid = getObject oid >>= maybe (raise E_INVARG) return

binaryString :: StrT -> MOO ByteString
binaryString = maybe (raise E_INVARG) (return . BS.pack) . text2binary

random :: (Random a) => (a, a) -> MOO a
random range = do
  g <- gets randomGen
  let (r, g') = randomR range g
  modify $ \st -> st { randomGen = g' }
  return r
