
{-# LANGUAGE OverloadedStrings #-}

module MOO.Task ( MOO
                , Task ( .. )
                , TaskDisposition ( .. )
                , Resume ( .. )
                , DelayedIO ( .. )
                , Environment ( .. )
                , TaskState ( .. )
                , CallStack ( .. )
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
                , modifyProperty
                , setBuiltinProperty
                , reader
                , local
                , initFrame
                , pushFrame
                , popFrame
                , frame
                , caller
                , modifyFrame
                , catchException
                , raiseException
                , notyet
                , raise
                , checkFloat
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

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.ByteString as BS

import MOO.Types
import {-# SOURCE #-} MOO.Database
import MOO.Object

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
                     | Abort Exception
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
  mempty  = DelayedIO $ return ()
  (DelayedIO a) `mappend` (DelayedIO b) = DelayedIO (a >> b)

delayIO :: IO () -> MOO ()
delayIO io = do
  existing <- gets delayedIO
  modify $ \st -> st { delayedIO = existing `mappend` (DelayedIO io) }

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
    , exceptionHandler = Handler $ interrupt . Abort
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

modifyProperty :: Object -> StrT -> (Property -> MOO Property) -> MOO ()
modifyProperty obj name f = do
  case lookupPropertyRef obj (T.toCaseFold name) of
    Nothing       -> raise E_PROPNF
    Just propTVar -> do
      prop  <- liftSTM $ readTVar propTVar
      prop' <- f prop
      liftSTM $ writeTVar propTVar prop'

setBuiltinProperty :: ObjId -> StrT -> Value -> MOO ()
setBuiltinProperty oid "name" (Str name) = do
  db <- getDatabase
  obj <- liftSTM (dbObject oid db) >>= maybe (raise E_INVIND) return
  if objectIsPlayer obj
    then checkWizard
    else checkPermission (objectOwner obj)
  liftSTM $ modifyObject oid db $ \obj -> return obj { objectName = name }
setBuiltinProperty oid "owner" (Obj owner) = do
  checkWizard
  db <- getDatabase
  liftSTM $ modifyObject oid db $ \obj -> return obj { objectOwner = owner }
setBuiltinProperty _ "location" (Obj _) = raise E_PERM
setBuiltinProperty _ "contents" (Lst _) = raise E_PERM
setBuiltinProperty oid "programmer" bit = do
  checkWizard
  db <- getDatabase
  liftSTM $ modifyObject oid db $ \obj ->
    return obj { objectProgrammer = truthOf bit }
setBuiltinProperty oid "wizard" bit = do
  checkWizard
  db <- getDatabase
  liftSTM $ modifyObject oid db $ \obj ->
    return obj { objectWizard = truthOf bit }
setBuiltinProperty oid "r" bit = do
  db <- getDatabase
  obj <- liftSTM (dbObject oid db) >>= maybe (raise E_INVIND) return
  checkPermission (objectOwner obj)
  liftSTM $ modifyObject oid db $ \obj ->
    return obj { objectPermR = truthOf bit }
setBuiltinProperty oid "w" bit = do
  db <- getDatabase
  obj <- liftSTM (dbObject oid db) >>= maybe (raise E_INVIND) return
  checkPermission (objectOwner obj)
  liftSTM $ modifyObject oid db $ \obj ->
    return obj { objectPermW = truthOf bit }
setBuiltinProperty oid "f" bit = do
  db <- getDatabase
  obj <- liftSTM (dbObject oid db) >>= maybe (raise E_INVIND) return
  checkPermission (objectOwner obj)
  liftSTM $ modifyObject oid db $ \obj ->
    return obj { objectPermF = truthOf bit }
setBuiltinProperty _ _ _ = raise E_TYPE

newtype CallStack = Stack [StackFrame]
                  deriving Show

newtype Continuation = Continuation (Value -> MOO Value)

instance Show Continuation where
  show _ = "<continuation>"

data StackFrame = Frame {
    continuation :: Continuation
  , variables    :: Map Id Value
  , debugBit     :: Bool
  , permissions  :: ObjId
} deriving Show

initFrame :: Bool -> ObjId -> StackFrame
initFrame debug programmer = Frame {
    continuation = Continuation return
  , variables    = mkInitVars
  , debugBit     = debug
  , permissions  = programmer
}

pushFrame :: StackFrame -> MOO ()
pushFrame frame = modify $ \st@State { stack = Stack frames } ->
  st { stack = Stack (frame : frames) }

popFrame :: Value -> MOO Value
popFrame value = do
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

newtype ExceptionHandler = Handler (Exception -> MOO Value)

data Exception = Exception Code Message Value
type Code = Value
type Message = StrT

catchException :: MOO a -> (Exception -> MOO a) -> MOO a
catchException action handler = callCC $ \k -> local (mkHandler k) action
  where mkHandler k env = env { exceptionHandler = Handler $ \e ->
                                 local (const env) $ handler e >>= k }

raiseException :: Exception -> MOO a
raiseException except = do
  Handler handler <- asks exceptionHandler
  handler except
  error "Returned from exception handler"

notyet = raiseException $
         Exception (Err E_INVARG) "Not yet implemented" (Int 0)

raise :: Error -> MOO a
raise err = raiseException $ Exception (Err err) (error2text err) (Int 0)

checkFloat :: FltT -> MOO Value
checkFloat flt | isInfinite flt = raise E_FLOAT
               | isNaN      flt = raise E_INVARG
               | otherwise      = return (Flt flt)

checkWizard' :: ObjId -> MOO ()
checkWizard' perm = do
  wizard <- fmap (maybe False objectWizard) $ getObject perm
  unless wizard $ raise E_PERM

checkWizard :: MOO ()
checkWizard = frame permissions >>= checkWizard'

checkPermission :: ObjId -> MOO ()
checkPermission who = do
  perm <- frame permissions
  if perm == who
    then return ()
    else checkWizard' perm

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
