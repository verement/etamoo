
{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}

module MOO.Task ( MOO
                , World(..)
                , Task(..)
                , TaskStatus(..)
                , TaskDisposition(..)
                , Resource(..)
                , Wake(..)
                , Resume(..)
                , DelayedIO(..)
                , Environment(..)
                , TaskState(..)
                , CallStack(..)
                , Continuation(..)
                , StackFrame(..)
                , Exception(..)
                , initWorld
                , initTask
                , newTaskId
                , newTask
                , taskOwner
                , isQueued
                , queuedTasks
                , timeoutException
                , stepTask
                , runTask
                , forkTask
                , interrupt
                , requestIO
                , liftSTM
                , initEnvironment
                , initState
                , newState
                , getWorld
                , getWorld'
                , putWorld
                , modifyWorld
                , getTask
                , putTask
                , purgeTask
                , getDatabase
                , putDatabase
                , getPlayer
                , getObject
                , getProperty
                , getVerb
                , findVerb
                , callCommandVerb
                , callVerb
                , callFromFunc
                , evalFromFunc
                , runVerb
                , runTick
                , modifyProperty
                , modifyVerb
                , readProperty
                , writeProperty
                , setBuiltinProperty
                , initFrame
                , formatFrames
                , pushFrame
                , popFrame
                , activeFrame
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
                , mkVariables
                , catchException
                , passException
                , raiseException
                , notyet
                , raise
                , isWizard
                , checkFloat
                , checkProgrammer
                , checkWizard
                , checkPermission
                , checkValid
                , checkFertile
                , checkRecurrence
                , binaryString
                , random
                , newRandomGen
                , formatTraceback
                , delayIO
                ) where

import Control.Arrow (first, (&&&))
import Control.Concurrent (ThreadId, myThreadId, forkIO, threadDelay,
                           newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (STM, TVar, atomically, retry,
                               newEmptyTMVar, putTMVar, takeTMVar,
                               readTVar, writeTVar, modifyTVar)
import Control.Monad (when, unless, join, liftM, void, (>=>))
import Control.Monad.Cont (ContT, runContT, callCC)
import Control.Monad.Reader (ReaderT, runReaderT, local, asks)
import Control.Monad.State.Strict (StateT, runStateT, get, gets, modify)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (execWriter, tell)
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Map (Map)
import Data.Maybe (isNothing, fromMaybe, fromJust)
import Data.Monoid (Monoid(mempty, mappend), (<>))
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Posix (nanosleep)
import System.Random (Random, StdGen, newStdGen, mkStdGen, split,
                      randomR, randomRs)

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

import MOO.Types
import {-# SOURCE #-} MOO.Database
import {-# SOURCE #-} MOO.Network
import MOO.Object
import MOO.Verb
import MOO.Command

-- | This is the basic MOO monad transformer stack. A computation of type
-- @'MOO' a@ is an 'STM' transaction that returns a value of type @a@ within
-- an environment that supports state, continuations, and local modification.
type MOO = ReaderT Environment
           (ContT TaskDisposition
            (StateT TaskState STM))

-- | Lift an 'STM' transaction into the 'MOO' monad.
liftSTM :: STM a -> MOO a
liftSTM = lift . lift . lift

-- | The known universe, as far as the MOO server is concerned
data World = World {
    database         :: Database                 -- ^ The database of objects
  , tasks            :: Map TaskId Task          -- ^ Queued and running tasks

  , listeners        :: Map PortNumber Listener  -- ^ Network listening points
  , connections      :: Map ObjId Connection     -- ^ Network connections

  , nextConnectionId :: ObjId
    -- ^ The (negative) object number to be assigned to the next inbound or
    -- outbound connection
  }

initWorld = World {
    database         = undefined
  , tasks            = M.empty

  , listeners        = M.empty
  , connections      = M.empty

  , nextConnectionId = -1
  }

-- | A structure representing a queued or running task
data Task = Task {
    taskId          :: TaskId
  , taskStatus      :: TaskStatus

  , taskThread      :: ThreadId
  , taskWorld       :: TVar World
  , taskPlayer      :: ObjId

  , taskState       :: TaskState
  , taskComputation :: MOO Value
}

instance Show Task where
  show task = "<Task " ++ show (taskId task) ++
              ": " ++ show (taskStatus task) ++ ">"

initTask = Task {
    taskId          = 0
  , taskStatus      = Pending

  , taskThread      = undefined
  , taskWorld       = undefined
  , taskPlayer      = -1

  , taskState       = initState
  , taskComputation = return nothing
  }

instance Sizeable Task where
  storageBytes task =
    storageBytes (taskId     task) +
    storageBytes (taskThread task) +
    storageBytes (taskWorld  task) +
    storageBytes (taskPlayer task) +
    storageBytes (taskStatus task) +
    storageBytes (taskState  task)
    -- storageBytes (taskComputation task)

instance Eq Task where
  Task { taskId = taskId1 } == Task { taskId = taskId2 } =
    taskId1 == taskId2

instance Ord Task where
  Task { taskState = state1 } `compare` Task { taskState = state2 } =
    startTime state1 `compare` startTime state2

type TaskId = Int

-- | Generate and return a (random) 'TaskId' not currently in use by any
-- existing task.
newTaskId :: World -> StdGen -> STM TaskId
newTaskId world gen =
  return $ fromJust $ find unused $ randomRs (1, maxBound) gen
  where unused taskId = M.notMember taskId (tasks world)

-- | Create a pending 'Task' for the given computation on behalf of the given
-- player. A new 'TaskId' is reserved for the task and the task is added to
-- the 'World'. (The task will not actually run until passed to 'runTask'.)
newTask :: TVar World -> ObjId -> MOO Value -> IO Task
newTask world' player comp = do
  gen <- newStdGen
  state <- newState

  atomically $ do
    world <- readTVar world'
    taskId <- newTaskId world gen

    let task = initTask {
            taskId          = taskId

          , taskWorld       = world'
          , taskPlayer      = player

          , taskState       = state
          , taskComputation = comp
          }

    writeTVar world' world { tasks = M.insert taskId task (tasks world) }
    return task

taskOwner :: Task -> ObjId
taskOwner = permissions . activeFrame

-- | The running state of a task
data TaskStatus = Pending | Running | Forked | Suspended Wake | Reading
                deriving Show

isQueued :: TaskStatus -> Bool
isQueued Pending = False
isQueued Running = False
isQueued _       = True

isRunning :: TaskStatus -> Bool
isRunning Running = True
isRunning _       = False

queuedTasks :: MOO [Task]
queuedTasks =
  (filter (isQueued . taskStatus) . M.elems . tasks) `liftM` getWorld

instance Sizeable TaskStatus where
  storageBytes (Suspended _) = 2 * storageBytes ()
  storageBytes _             =     storageBytes ()

-- | A function to call in order to wake a suspended task
newtype Wake = Wake (Value -> IO ())

instance Show Wake where
  show _ = "Wake{..}"

-- | The intermediate or final result of a running task
data TaskDisposition = Complete Value
                     | Suspend (Maybe Integer)    (Resume ())
                     | Read     ObjId             (Resume Value)
                     | forall a. RequestIO (IO a) (Resume a)
                     | Uncaught Exception CallStack
                     | Timeout  Resource  CallStack
                     | Suicide

-- | A continuation to resume the execution of a task where it left off
newtype Resume a = Resume (a -> MOO Value)

-- | Task resource limits
data Resource = Ticks | Seconds

showResource :: Resource -> Text
showResource Ticks   = "ticks"
showResource Seconds = "seconds"

timeoutException :: Resource -> Exception
timeoutException resource =
  Exception (Err E_QUOTA) ("Task ran out of " <> showResource resource) nothing

stepTask :: Task -> IO (TaskDisposition, Task)
stepTask task = do
  let env    = initEnvironment task
      comp   = taskComputation task
      comp'  = callCC $ \k ->
        Complete `liftM` local (\r -> r { interruptHandler = Interrupt k }) comp
      state  = taskState task
      contM  = runReaderT comp' env
      stateM = runContT contM return
      stmM   = runStateT stateM state
  (result, state') <- atomically stmM
  runDelayed $ delayedIO state'
  return (result, task { taskState = state' { delayedIO = mempty }})

stepTaskWithIO :: Task -> IO (TaskDisposition, Task)
stepTaskWithIO task = do
  (disposition, task') <- stepTask task
  case disposition of
    RequestIO io (Resume resume) -> do
      result <- io
      stepTaskWithIO task' { taskComputation = resume result }
    _ -> return (disposition, task')

-- | Run a task in a new Haskell thread, returning either the value produced
-- by the task, or 'Nothing' if the task suspends or aborts before producing a
-- value. If the task suspends, it may continue running after this function
-- returns. The task is removed from the task queue after it is finished.
runTask :: Task -> IO (Maybe Value)
runTask task = do
  resultMVar <- newEmptyMVar

  forkIO $ do
    threadId <- myThreadId
    let task' = task { taskThread = threadId }

    atomically $ modifyTVar (taskWorld task) $ \world ->
      world { tasks = M.insert (taskId task)
                      task' { taskStatus = Running } $ tasks world }

    runTask' task' $ putMVar resultMVar

    atomically $ modifyTVar (taskWorld task) $ \world ->
      world { tasks = M.delete (taskId task) $ tasks world }

  takeMVar resultMVar

  where noOp = const $ return ()

        runTask' :: Task -> (Maybe Value -> IO ()) -> IO ()
        runTask' task putResult = do
          (disposition, task') <- stepTaskWithIO task
          case disposition of
            Complete value -> putResult (Just value)

            Suspend _ (Resume resume) -> do
              putResult Nothing

              -- restart this task only when there are none other running
              atomically $ do
                world <- readTVar (taskWorld task')
                when (any (isRunning . taskStatus) $ M.elems $ tasks world)
                  retry

              runTask' task' { taskComputation = resume () } noOp

            Read _ _ -> error "read() not yet implemented"

            Uncaught exception@(Exception code message value)
              stack@(Stack frames) ->
              handleAbortedTask task' formatted putResult $
                callSystemVerb "handle_uncaught_error"
                [code, Str message, value, traceback, stringList formatted]
              where traceback = formatFrames True frames
                    formatted = formatTraceback exception stack

            Timeout resource stack@(Stack frames) ->
              handleAbortedTask task' formatted putResult $
                callSystemVerb "handle_task_timeout"
                [Str $ showResource resource, traceback, stringList formatted]
              where traceback = formatFrames True frames
                    formatted = formatTraceback
                                (timeoutException resource) stack

            Suicide -> putResult Nothing

        handleAbortedTask :: Task -> [Text] -> (Maybe Value -> IO ()) ->
                             MOO (Maybe Value) -> IO ()
        handleAbortedTask task traceback putResult call = do
          state <- newState
          handleAbortedTask' traceback task {
              taskState = state
            , taskComputation = fromMaybe nothing `fmap` call
            }

          where handleAbortedTask' :: [Text] -> Task -> IO ()
                handleAbortedTask' traceback task = do
                  (disposition, task') <- stepTaskWithIO task
                  case disposition of
                    Complete value -> do
                      unless (truthOf value) $ informPlayer traceback
                      putResult Nothing
                    Suspend _ (Resume resume) -> do
                      -- The aborted task is considered "handled" but continue
                      -- running the suspended handler (which might abort
                      -- again!)
                      putResult Nothing
                      runTask' task' { taskComputation = resume () } noOp
                    Read _ _ -> error "read() not yet implemented"
                    Uncaught exception stack -> do
                      informPlayer traceback
                      informPlayer $ formatTraceback exception stack
                      putResult Nothing
                    Timeout resource stack -> do
                      informPlayer traceback
                      informPlayer $ formatTraceback
                        (timeoutException resource) stack
                      putResult Nothing
                    Suicide -> putResult Nothing

                  where informPlayer :: [Text] -> IO ()
                        informPlayer = mapM_ (putStrLn . T.unpack)  -- XXX

-- | Create and queue a task to run the given computation after the given
-- microsecond delay. 'E_INVARG' may be raised if the delay is out of
-- acceptable range. (The given 'TaskId' should have been reserved by a call
-- to 'newTaskId'.)
forkTask :: TaskId -> Integer -> MOO Value -> MOO ()
forkTask taskId usecs code = do
  state <- get

  let now = startTime state
      estimatedWakeup = (fromIntegral usecs / 1000000) `addUTCTime` now

  when (estimatedWakeup < now || estimatedWakeup > endOfTime) $ raise E_INVARG

  task <- asks task
  gen <- newRandomGen

  let frame = currentFrame (stack state)

      frame' = frame {
          depthLeft    = depthLeft initFrame
        , contextStack = contextStack initFrame
        , lineNumber   = lineNumber frame + 1
        }

      state' = initState {
          ticksLeft = 15000  -- XXX
        , stack     = Stack [frame']
        , startTime = estimatedWakeup
        , randomGen = gen
        }

      task' = task {
          taskId          = taskId
        , taskStatus      = Forked
        , taskState       = state'
        , taskComputation = code
        }

  -- make sure the forked task doesn't start before the current task commits
  startSignal <- liftSTM newEmptyTMVar

  threadId <- requestIO $ forkIO $ do
    if usecs <= fromIntegral (maxBound :: Int)
      then threadDelay (fromIntegral usecs)
      else nanosleep (usecs * 1000)
    atomically $ takeTMVar startSignal
    now <- getCurrentTime
    void $ runTask task' { taskState = state' { startTime = now } }

  modifyWorld $ \world ->
    world { tasks = M.insert taskId task' { taskThread = threadId } $
                    tasks world }
  liftSTM $ putTMVar startSignal ()

-- | A continuation for returning to the task dispatcher to handle an
-- interrupt request. Note that calling this continuation implies a commit to
-- the current task's transaction.
newtype InterruptHandler = Interrupt (TaskDisposition -> MOO TaskDisposition)

-- | Commit the current task's transaction, and return to the task dispatcher
-- with an interrupt request. The task dispatcher may resume execution of the
-- task later if the request is one which supplies an appropriate
-- continuation.
interrupt :: TaskDisposition -> MOO a
interrupt disp = do
  Interrupt handler <- asks interruptHandler
  handler disp
  error "Returned from interrupt handler"

-- | An 'IO' computation to be performed after the current task commits its
-- 'STM' transaction
newtype DelayedIO = DelayedIO { runDelayed :: IO () }

instance Monoid DelayedIO where
  mempty = DelayedIO $ return ()
  DelayedIO a `mappend` DelayedIO b = DelayedIO (a >> b)

-- | Interrupt the current task to perform the given IO computation, and
-- return the result. Note this implies a commit of the task's 'STM'
-- transaction.
requestIO :: IO a -> MOO a
requestIO io = callCC $ interrupt . RequestIO io . Resume

-- | Perform the given IO computation after the current task commits its 'STM'
-- transaction.
--
-- Since IO can't be performed within a transaction, this is a simple
-- alternative when the value returned by the IO isn't needed.
delayIO :: IO () -> MOO ()
delayIO io = modify $ \state ->
  state { delayedIO = delayedIO state `mappend` DelayedIO io }

-- | A 'Reader' environment for state that either doesn't change, or can be
-- locally modified for subcomputations
data Environment = Env {
    task             :: Task
  , interruptHandler :: InterruptHandler
  , exceptionHandler :: ExceptionHandler
  , indexLength      :: MOO Int
}

initEnvironment :: Task -> Environment
initEnvironment task = Env {
    task             = task
  , interruptHandler = error "Undefined interrupt handler"
  , exceptionHandler = Handler $ \e cs -> interrupt (Uncaught e cs)
  , indexLength      = error "Invalid index context"
  }

-- | A 'State' structure for data that may normally change during computation
data TaskState = State {
    ticksLeft :: Int
  , stack     :: CallStack
  , startTime :: UTCTime
  , randomGen :: StdGen
  , delayedIO :: DelayedIO
}

initState = State {
    ticksLeft = 30000
  , stack     = Stack []
  , startTime = posixSecondsToUTCTime 0
  , randomGen = mkStdGen 0
  , delayedIO = mempty
  }

instance Sizeable TaskState where
  storageBytes state =
    storageBytes (ticksLeft state) +
    storageBytes (stack     state) +
    storageBytes (startTime state) +
    storageBytes (randomGen state)
    -- storageBytes (delayedIO state)

newState :: IO TaskState
newState = do
  startTime <- getCurrentTime
  gen <- newStdGen
  return initState {
      startTime = startTime
    , randomGen = gen
    }

getWorld :: MOO World
getWorld = liftSTM . readTVar . taskWorld =<< asks task

getWorld' :: MOO (TVar World)
getWorld' = asks (taskWorld . task)

putWorld :: World -> MOO ()
putWorld world = do
  world' <- getWorld'
  liftSTM $ writeTVar world' world

modifyWorld :: (World -> World) -> MOO ()
modifyWorld f = do
  world' <- getWorld'
  liftSTM $ modifyTVar world' f

getTask :: TaskId -> MOO (Maybe Task)
getTask taskId = (M.lookup taskId . tasks) `liftM` getWorld

putTask :: Task -> MOO ()
putTask task = modifyWorld $ \world ->
  world { tasks = M.insert (taskId task) task $ tasks world }

purgeTask :: Task -> MOO ()
purgeTask task = modifyWorld $ \world ->
  world { tasks = M.delete (taskId task) $ tasks world }

getDatabase :: MOO Database
getDatabase = database `liftM` getWorld

putDatabase :: Database -> MOO ()
putDatabase db = modifyWorld $ \world -> world { database = db }

getPlayer :: MOO ObjId
getPlayer = asks (taskPlayer . task)

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

findVerb :: (Verb -> Bool) -> StrT -> ObjId -> MOO (Maybe ObjId, Maybe Verb)
findVerb acceptable name = findVerb'
  where findVerb' oid = do
          maybeObj <- getObject oid
          case maybeObj of
            Nothing  -> return (Nothing, Nothing)
            Just obj -> do
              maybeVerb <- searchVerbs (objectVerbs obj)
              case maybeVerb of
                Just verb -> return (Just oid, Just verb)
                Nothing   -> maybe (return (Just oid, Nothing))
                             findVerb' (objectParent obj)

        searchVerbs ((names,verbTVar):rest) =
          if verbNameMatch name' names
          then do
            verb <- liftSTM $ readTVar verbTVar
            if acceptable verb
              then return (Just verb)
              else searchVerbs rest
          else searchVerbs rest
        searchVerbs [] = return Nothing

        name' = T.toCaseFold name

callSystemVerb :: Id -> [Value] -> MOO (Maybe Value)
callSystemVerb name args = do
  player <- asks (taskPlayer . task)
  (maybeOid, maybeVerb) <- findVerb verbPermX name systemObject
  case (maybeOid, maybeVerb) of
    (Just verbOid, Just verb) -> do
      let vars = mkVariables [
              ("player", Obj player)
            , ("this"  , Obj systemObject)
            , ("verb"  , Str name)
            , ("args"  , fromList args)
            ]
      Just `liftM` runVerb verb initFrame {
          variables     = vars
        , verbName      = name
        , verbLocation  = verbOid
        , initialThis   = systemObject
        , initialPlayer = player
        }
    _ -> return Nothing

callCommandVerb :: ObjId -> (ObjId, Verb) -> ObjId ->
                   Command -> (ObjId, ObjId) -> MOO Value
callCommandVerb player (verbOid, verb) this command (dobj, iobj) = do
  let vars = mkVariables [
          ("player" , Obj player)
        , ("this"   , Obj this)
        , ("caller" , Obj player)
        , ("verb"   , Str        $ commandVerb    command)
        , ("argstr" , Str        $ commandArgStr  command)
        , ("args"   , stringList $ commandArgs    command)
        , ("dobjstr", Str        $ commandDObjStr command)
        , ("dobj"   , Obj dobj)
        , ("prepstr", Str        $ commandPrepStr command)
        , ("iobjstr", Str        $ commandIObjStr command)
        , ("iobj"   , Obj iobj)
        ]

  runVerb verb initFrame {
      variables     = vars
    , verbName      = commandVerb command
    , verbLocation  = verbOid
    , initialThis   = this
    , initialPlayer = player
    }

callVerb' :: (ObjId, Verb) -> ObjId -> StrT -> [Value] -> MOO Value
callVerb' (verbOid, verb) this name args = do
  thisFrame <- frame id
  wizard <- isWizard (permissions thisFrame)
  let player = if wizard
               then case vars M.! "player" of
                 (Obj oid) -> oid
                 _         -> initialPlayer thisFrame
               else initialPlayer thisFrame
      vars   = variables thisFrame
      vars'  = mkVariables [
          ("this"   , Obj this)
        , ("verb"   , Str name)
        , ("args"   , fromList args)
        , ("caller" , Obj $ initialThis thisFrame)
        , ("player" , Obj player)
        , ("argstr" , vars M.! "argstr")
        , ("dobjstr", vars M.! "dobjstr")
        , ("dobj"   , vars M.! "dobj")
        , ("prepstr", vars M.! "prepstr")
        , ("iobjstr", vars M.! "iobjstr")
        , ("iobj"   , vars M.! "iobj")
        ]

  runVerb verb initFrame {
      variables     = vars'
    , verbName      = name
    , verbLocation  = verbOid
    , initialThis   = this
    , initialPlayer = player
    }

callVerb :: ObjId -> ObjId -> StrT -> [Value] -> MOO Value
callVerb verbLoc this name args = do
  (maybeOid, maybeVerb) <- findVerb verbPermX name verbLoc
  case (maybeOid, maybeVerb) of
    (Nothing     , _)         -> raise E_INVIND
    (Just _      , Nothing)   -> raise E_VERBNF
    (Just verbOid, Just verb) -> callVerb' (verbOid, verb) this name args

callFromFunc :: Id -> IntT -> (ObjId, StrT) -> [Value] -> MOO (Maybe Value)
callFromFunc func index (oid, name) args = do
  (maybeOid, maybeVerb) <- findVerb verbPermX name oid
  case (maybeOid, maybeVerb) of
    (Just verbOid, Just verb) -> liftM Just $ evalFromFunc func index $
                                 callVerb' (verbOid, verb) oid name args
    (_           , _)         -> return Nothing

evalFromFunc :: Id -> IntT -> MOO Value -> MOO Value
evalFromFunc func index code = do
  (depthLeft, player) <- frame (depthLeft &&& initialPlayer)
  pushFrame initFrame {
      depthLeft     = depthLeft
    , verbName      = func
    , initialPlayer = player
    , builtinFunc   = True
    , lineNumber    = index
    }
  value <- code `catchException` \except callStack -> do
    popFrame
    passException except callStack
  popFrame
  return value

runVerb :: Verb -> StackFrame -> MOO Value
runVerb verb verbFrame = do
  Stack frames <- gets stack
  let depthLeft' = depthLeft $ case frames of
        frame:_ -> frame
        []      -> initFrame
  unless (depthLeft' > 0) $ raise E_MAXREC

  pushFrame verbFrame {
      depthLeft    = depthLeft' - 1
    , debugBit     = verbPermD verb
    , permissions  = verbOwner verb
    , verbFullName = verbNames verb
    }
  value <- verbCode verb `catchException` \except callStack -> do
    popFrame
    passException except callStack
  popFrame

  return value

runTick :: MOO ()
runTick = do
  ticksLeft <- gets ticksLeft
  unless (ticksLeft > 0) $ interrupt . Timeout Ticks =<< gets stack
  modify $ \state -> state { ticksLeft = ticksLeft - 1 }

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
        liftSTM $ modifyObject oid db $ replaceVerb index verb'

readProperty :: ObjId -> StrT -> MOO (Maybe Value)
readProperty oid name = do
  maybeObj <- getObject oid
  case maybeObj of
    Nothing  -> return Nothing
    Just obj -> maybe (search obj) (return . Just . ($ obj)) $
                builtinProperty name
  where search obj = do
          maybeProp <- liftSTM $ lookupProperty obj name
          case maybeProp of
            Nothing   -> return Nothing
            Just prop -> case propertyValue prop of
              Nothing -> do
                parentObj <- maybe (return Nothing) getObject (objectParent obj)
                maybe (error $ "No inherited value for property " ++
                       T.unpack name) search parentObj
              just -> return just

writeProperty :: ObjId -> StrT -> Value -> MOO ()
writeProperty oid name value = do
  maybeObj <- getObject oid
  case maybeObj of
    Nothing  -> return ()
    Just obj ->
      if isBuiltinProperty name
      then setBuiltinProperty (oid, obj) name value
      else case lookupPropertyRef obj name of
        Nothing       -> return ()
        Just propTVar -> liftSTM $ do
          prop <- readTVar propTVar
          writeTVar propTVar prop { propertyValue = Just value }

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

-- | The stack of verb and/or built-in function frames
newtype CallStack = Stack [StackFrame]
                  deriving Show

instance Sizeable CallStack where
  storageBytes (Stack stack) = storageBytes stack

-- | A local continuation for loop constructs
newtype Continuation = Continuation (Value -> MOO Value)

instance Sizeable Continuation where
  storageBytes _ = storageBytes ()

instance Show Continuation where
  show _ = "<Continuation>"

-- | A structure describing a (possibly nested) context for the current frame,
-- used to manage loop break/continue and try/finally interactions
data Context =
  Loop {
    loopName     :: Maybe Id
  , loopBreak    :: Continuation
  , loopContinue :: Continuation
  } |
  TryFinally {
    finally      :: MOO Value
  }

instance Sizeable Context where
  storageBytes context@Loop{} =
    storageBytes (loopName     context) +
    storageBytes (loopBreak    context) +
    storageBytes (loopContinue context)
  storageBytes TryFinally{} = storageBytes ()
    -- storageBytes (finally context)

instance Show Context where
  show Loop { loopName = Nothing   } = "<Loop>"
  show Loop { loopName = Just name } = "<Loop " ++ show name ++ ">"

  show TryFinally{} = "<TryFinally>"

-- | The data tracked for each verb and/or built-in function call
data StackFrame = Frame {
    depthLeft     :: Int

  , contextStack  :: [Context]
  , variables     :: Map Id Value
  , debugBit      :: Bool
  , permissions   :: ObjId

  , verbName      :: StrT
  , verbFullName  :: StrT
  , verbLocation  :: ObjId
  , initialThis   :: ObjId
  , initialPlayer :: ObjId

  , builtinFunc   :: Bool
  , lineNumber    :: IntT
} deriving Show

initFrame = Frame {
    depthLeft     = 50

  , contextStack  = []
  , variables     = initVariables
  , debugBit      = True
  , permissions   = -1

  , verbName      = T.empty
  , verbFullName  = T.empty
  , verbLocation  = -1
  , initialThis   = -1
  , initialPlayer = -1

  , builtinFunc   = False
  , lineNumber    = 0
}

instance Sizeable StackFrame where
  storageBytes frame =
    storageBytes (depthLeft     frame) +
    storageBytes (contextStack  frame) +
    storageBytes (variables     frame) +
    storageBytes (debugBit      frame) +
    storageBytes (permissions   frame) +
    storageBytes (verbName      frame) +
    storageBytes (verbFullName  frame) +
    storageBytes (verbLocation  frame) +
    storageBytes (initialThis   frame) +
    storageBytes (initialPlayer frame) +
    storageBytes (builtinFunc   frame) +
    storageBytes (lineNumber    frame)

formatFrames :: Bool -> [StackFrame] -> Value
formatFrames includeLineNumbers = fromListBy formatFrame
  where formatFrame frame = fromList $
                              Obj (initialThis   frame)
                            : Str (verbName      frame)
                            : Obj (permissions   frame)
                            : Obj (verbLocation  frame)
                            : Obj (initialPlayer frame)
                            : [Int $ lineNumber frame | includeLineNumbers]

pushFrame :: StackFrame -> MOO ()
pushFrame frame = modify $ \state@State { stack = Stack frames } ->
  state { stack = Stack (frame : frames) }

popFrame :: MOO ()
popFrame = do
  unwindContexts (const False)
  modify $ \state@State { stack = Stack (_:frames) } ->
    state { stack = Stack frames }

currentFrame :: CallStack -> StackFrame
currentFrame (Stack (frame:_)) = frame
currentFrame (Stack [])        = error "currentFrame: Empty call stack"

previousFrame :: CallStack -> Maybe StackFrame
previousFrame (Stack (_:frames)) = previousFrame' frames
  where previousFrame' (frame:frames)
          | builtinFunc frame = previousFrame' frames
          | otherwise         = Just frame
        previousFrame' [] = Nothing
previousFrame (Stack []) = error "previousFrame: Empty call stack"

activeFrame :: Task -> StackFrame
activeFrame = currentFrame . stack . taskState

frame :: (StackFrame -> a) -> MOO a
frame f = gets (f . currentFrame . stack)

caller :: (StackFrame -> a) -> MOO (Maybe a)
caller f = gets (fmap f . previousFrame . stack)

modifyFrame :: (StackFrame -> StackFrame) -> MOO ()
modifyFrame f = modify $ \state@State { stack = Stack (frame:stack) } ->
  state { stack = Stack (f frame : stack) }

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

-- | The default collection of verb variables
initVariables :: Map Id Value
initVariables = M.fromList $ [
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
  ] ++ typeVariables

  where typeVariables = map (first T.toCaseFold) [
            ("INT"  , Int $ typeCode TInt)
          , ("NUM"  , Int $ typeCode TInt)
          , ("FLOAT", Int $ typeCode TFlt)
          , ("LIST" , Int $ typeCode TLst)
          , ("STR"  , Int $ typeCode TStr)
          , ("OBJ"  , Int $ typeCode TObj)
          , ("ERR"  , Int $ typeCode TErr)
          ]

-- | Create a variable block for a verb by overriding the default.
mkVariables :: [(Id, Value)] -> Map Id Value
mkVariables = foldr (uncurry M.insert) initVariables

newtype ExceptionHandler = Handler (Exception -> CallStack -> MOO Value)

instance Show ExceptionHandler where
  show _ = "<ExceptionHandler>"

-- | A MOO exception
data Exception = Exception Code Message Value
type Code = Value
type Message = StrT

-- | Install a local exception handler for the duration of the passed
-- computation.
catchException :: MOO a -> (Exception -> CallStack -> MOO a) -> MOO a
catchException action handler = callCC $ \k -> local (mkHandler k) action
  where mkHandler k env = env { exceptionHandler = Handler $ \e cs ->
                                 local (const env) $ handler e cs >>= k }

-- | Re-raise an exception to the next enclosing handler.
passException :: Exception -> CallStack -> MOO a
passException except callStack = do
  Handler handler <- asks exceptionHandler
  handler except callStack
  error "Returned from exception handler"

-- | Abort execution of the current computation and call the closest enclosing
-- exception handler.
raiseException :: Exception -> MOO a
raiseException except = passException except =<< gets stack

-- | Placeholder for features not yet implemented
notyet :: String -> MOO a
notyet what = raiseException $
              Exception (Err E_QUOTA) "Not yet implemented" (Str $ T.pack what)

-- | Create and raise an exception for the given MOO error.
raise :: Error -> MOO a
raise err = raiseException $ Exception (Err err) (error2text err) nothing

-- | Verify that the given floating point number is neither infinite nor NaN,
-- raising 'E_FLOAT' or 'E_INVARG' respectively if so. Also, return the
-- corresponding MOO value.
checkFloat :: FltT -> MOO Value
checkFloat flt
  | isInfinite flt = raise E_FLOAT
  | isNaN      flt = raise E_INVARG
  | otherwise      = return (Flt flt)

-- | Verify that the given object has a programmer bit, raising 'E_PERM' if
-- not.
checkProgrammer' :: ObjId -> MOO ()
checkProgrammer' perm = do
  programmer <- maybe False objectProgrammer `liftM` getObject perm
  unless programmer $ raise E_PERM

-- | Verify that the current task permissions have programmer privileges,
-- raising 'E_PERM' if not.
checkProgrammer :: MOO ()
checkProgrammer = checkProgrammer' =<< frame permissions

-- | Determine whether the given object has its wizard bit set.
isWizard :: ObjId -> MOO Bool
isWizard = liftM (maybe False objectWizard) . getObject

-- | Verify that the given object is a wizard, raising 'E_PERM' if not.
checkWizard' :: ObjId -> MOO ()
checkWizard' perm = do
  wizard <- isWizard perm
  unless wizard $ raise E_PERM

-- | Verify that the current task permissions have wizard privileges, raising
-- 'E_PERM' if not.
checkWizard :: MOO ()
checkWizard = checkWizard' =<< frame permissions

-- | Verify that the current task permissions either have wizard privileges or
-- are the same as the given object, raising 'E_PERM' if not.
checkPermission :: ObjId -> MOO ()
checkPermission who = do
  perm <- frame permissions
  unless (perm == who) $ checkWizard' perm

-- | Verify that the given object is valid, raising 'E_INVARG' if not. Also,
-- return the referenced object.
checkValid :: ObjId -> MOO Object
checkValid = getObject >=> maybe (raise E_INVARG) return

-- | Verify that the given object is fertile for the current task permissions,
-- raising 'E_PERM' if not.
checkFertile :: ObjId -> MOO ()
checkFertile oid = do
  maybeObj <- getObject oid
  case maybeObj of
    Nothing  -> raise E_PERM
    Just obj -> unless (objectPermF obj) $ checkPermission (objectOwner obj)

-- | Verify that the given /object/ does not have a recursive relationship
-- with the given /subject/, raising 'E_RECMOVE' if so.
checkRecurrence :: (Object -> Maybe ObjId)  -- ^ relationship projection
                -> ObjId                    -- ^ /subject/
                -> ObjId                    -- ^ /object/ to check
                -> MOO ()
checkRecurrence relation subject = checkRecurrence'
  where checkRecurrence' object = do
          when (object == subject) $ raise E_RECMOVE
          maybeObject <- getObject object
          case join $ relation `fmap` maybeObject of
            Just oid -> checkRecurrence' oid
            Nothing  -> return ()

-- | Translate a MOO /binary string/ into a Haskell 'ByteString', raising
-- 'E_INVARG' if the MOO string is improperly formatted.
binaryString :: StrT -> MOO ByteString
binaryString = maybe (raise E_INVARG) (return . BS.pack) . text2binary

-- | Generate and return a pseudorandom number in the given range, modifying
-- the local generator state.
random :: (Random a) => (a, a) -> MOO a
random range = do
  (r, gen) <- randomR range `liftM` gets randomGen
  modify $ \state -> state { randomGen = gen }
  return r

-- | Split the local random number generator state in two, updating the local
-- state with one of them and returning the other.
newRandomGen :: MOO StdGen
newRandomGen = do
  (gen, gen') <- split `liftM` gets randomGen
  modify $ \state -> state { randomGen = gen }
  return gen'

-- | Generate traceback lines for an exception, suitable for displaying to a
-- user.
formatTraceback :: Exception -> CallStack -> [Text]
formatTraceback (Exception _ message _) (Stack frames) =
  T.splitOn "\n" $ execWriter (traceback frames)

  where traceback (frame:frames) = do
          describeVerb frame
          tell $ ":  " <> message
          traceback' frames
        traceback [] = traceback' []

        traceback' (frame:frames) = do
          tell "\n... called from "
          describeVerb frame
          traceback' frames
        traceback' [] = tell "\n(End of traceback)"

        describeVerb Frame { builtinFunc = False
                           , verbLocation = loc, verbFullName = name
                           , initialThis = this, lineNumber = line } = do
          tell $ "#" <> T.pack (show loc) <> ":" <> name
          when (loc /= this) $ tell $ " (this == #" <> T.pack (show this) <> ")"
          when (line > 0) $ tell $ ", line " <> T.pack (show line)
        describeVerb Frame { builtinFunc = True, verbName = name } =
          tell $ "built-in function " <> name <> "()"
