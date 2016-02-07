
{-# LANGUAGE OverloadedStrings #-}

module MOO.Builtins.Tasks (builtins) where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (atomically, newEmptyTMVar, takeTMVar, putTMVar)
import Control.Monad (void)
import Control.Monad.Cont (callCC)
import Control.Monad.Reader (asks)
import Control.Monad.State (gets, modify, get)
import Data.List (sort)
import Data.Time (getCurrentTime, addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.VCache (vref)

import qualified Data.HashMap.Lazy as HM
import qualified Data.Set as S

import MOO.Builtins.Common
import {-# SOURCE #-} MOO.Builtins
import MOO.Object
import MOO.Parser
import MOO.Task
import MOO.Types
import MOO.Verb

import qualified MOO.String as Str

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- | § 4.4.6 MOO-Code Evaluation and Task Manipulation
builtins :: [Builtin]
builtins = [
    bf_raise
  , bf_call_function
  , bf_function_info
  , bf_eval
  , bf_set_task_perms
  , bf_caller_perms
  , bf_ticks_left
  , bf_seconds_left
  , bf_task_id
  , bf_suspend
  , bf_resume
  , bf_queue_info
  , bf_queued_tasks
  , bf_kill_task
  , bf_callers
  , bf_task_stack
  ]

bf_raise = Builtin "raise" 1 (Just 3)
           [TAny, TStr, TAny] TAny $ \(code : optional) ->
  let [Str message, value] =
        defaults optional [Str $ Str.fromText $ toText code, zero]
  in raiseException code message value

bf_call_function = Builtin "call_function" 1 Nothing
                   [TStr] TAny $ \(Str func_name : args) ->
  callBuiltin (toId func_name) args

bf_function_info = Builtin "function_info" 0 (Just 1)
                   [TStr] TLst $ \args -> case args of
  []         -> return $ fromListBy formatInfo $ HM.elems builtinFunctions
  [Str name] -> maybe (raise E_INVARG) (return . formatInfo) $
                HM.lookup (toId name) builtinFunctions

  where formatInfo :: Builtin -> Value
        formatInfo Builtin { builtinName     = name
                           , builtinMinArgs  = min
                           , builtinMaxArgs  = max
                           , builtinArgTypes = types
                           } =
          fromList [ Str $ fromId name
                   , Int $ fromIntegral min
                   , Int $ maybe (-1) fromIntegral max
                   , fromListBy (Int . typeCode) types
                   ]

bf_eval = Builtin "eval" 1 (Just 1) [TStr] TLst $ \[Str string] ->
  checkProgrammer >> case parseProgram (Str.toText string) of
    Right program -> do
      (programmer, this, player) <- frame $ \frame ->
        (permissions frame, initialThis frame, initialPlayer frame)
      vspace <- getVSpace
      let verb = (newVerb vspace) { verbNames   = "Input to EVAL"
                                  , verbProgram = vref vspace program
                                  , verbOwner   = programmer
                                  , verbPermD   = True
                                  }
          vars = mkVariables [ ("player", Obj player)
                             , ("caller", Obj this)
                             ]
      value <- evalFromFunc "eval" 0 $
               runVerb verb initFrame { variables     = vars
                                      , initialPlayer = player
                                      }
      return $ fromList [truthValue True, value]
    Left errors ->
      return $ fromList [truthValue False,
                         fromListBy (Str . Str.fromString) errors]

bf_set_task_perms = Builtin "set_task_perms" 1 (Just 1)
                    [TObj] TAny $ \[Obj who] -> do
  checkPermission who
  modifyFrame $ \frame -> frame { permissions = who }
  return zero

bf_caller_perms = Builtin "caller_perms" 0 (Just 0) [] TObj $ \[] ->
  Obj . objectForMaybe <$> caller permissions

bf_ticks_left = Builtin "ticks_left" 0 (Just 0) [] TInt $ \[] ->
  Int . fromIntegral <$> gets ticksLeft

bf_seconds_left = Builtin "seconds_left" 0 (Just 0) [] TInt $ \[] -> do
  (limit, start) <- gets (secondsLimit &&& startTime)
  -- We calculate the elapsed time within the following IO action rather than
  -- simply return the current time so that the action depends on some local
  -- state and thus will be re-executed each time and not be optimized away to
  -- a constant. Unsafe indeed!
  elapsed <- unsafeIOtoMOO $ (`diffUTCTime` start) <$> getCurrentTime
  return $ Int $ fromIntegral $ limit - round elapsed

bf_task_id = Builtin "task_id" 0 (Just 0) [] TInt $ \[] ->
  Int . fromIntegral <$> asks (taskId . task)

bf_suspend = Builtin "suspend" 0 (Just 1) [TNum] TAny $ \optional -> do
  let (maybeSeconds : _) = maybeDefaults optional

  {- maybeMicrosecs <- mapM getDelay maybeSeconds  -- requires GHC 7.10 -}
  maybeMicrosecs <- maybe (return Nothing) (fmap Just . getDelay) maybeSeconds

  state <- get

  estimatedWakeup <- case maybeMicrosecs of
    Just usecs
      | time < now || time > endOfTime -> raise E_INVARG
      | otherwise                      -> return time
      where now  = startTime state
            time = (fromIntegral usecs / 1000000) `addUTCTime` now

    Nothing -> return endOfTime  -- XXX this is a sad wart in need of remedy

  checkQueuedTaskLimit

  resumeTMVar <- liftSTM newEmptyTMVar
  task <- asks task

  let wake value = do
        now <- getCurrentTime
        atomically $ putTMVar resumeTMVar (now, value)

      task' = task {
          taskStatus = Suspended (Wake wake)
        , taskState  = state { startTime = estimatedWakeup }
        }

  case maybeMicrosecs of
    Just usecs -> delayIO $ void $ forkIO $ delay usecs >> wake zero
    Nothing    -> return ()

  putTask task'

  callCC $ interrupt . Suspend . Resume
  (now, value) <- liftSTM $ takeTMVar resumeTMVar

  putTask task' { taskStatus = Running }

  resetLimits False
  modify $ \state -> state { startTime = now }

  case value of
    Err error -> raise error
    _         -> return value

bf_resume = Builtin "resume" 1 (Just 2)
            [TInt, TAny] TAny $ \(Int task_id : optional) -> do
  let [value] = defaults optional [zero]

  maybeTask <- getTask (fromIntegral task_id)
  case maybeTask of
    Just task@Task { taskStatus = Suspended (Wake wake) } -> do
      checkPermission (taskOwner task)
      putTask task { taskStatus = Running }
      delayIO (wake value)
    _ -> raise E_INVARG

  return zero

bf_queue_info = Builtin "queue_info" 0 (Just 1) [TObj] TAny $ \args ->
  let info = case args of
        []           -> objectList . S.toList .
                        foldr (S.insert . taskOwner) S.empty
        [Obj player] -> Int . fromIntegral . length .
                        filter ((== player) . taskOwner)
  in info <$> queuedTasks

bf_queued_tasks = Builtin "queued_tasks" 0 (Just 0) [] TLst $ \[] -> do
  tasks <- queuedTasks
  programmer <- frame permissions
  wizard <- isWizard programmer
  let ownedTasks | wizard    = tasks
                 | otherwise = filter ((== programmer) . taskOwner) tasks

  return $ fromListBy formatTask $ sort ownedTasks

  where formatTask :: Task -> Value
        formatTask task = fromListBy ($ task) [
            Int . fromIntegral . taskId        -- task-id
          , Int . floor . utcTimeToPOSIXSeconds .
                  startTime . taskState        -- start-time
          , const (Int 0)                      -- clock-id    (obsolete)
          , const (Int defaultBgTicks)         -- clock-ticks (obsolete)
          , Obj . taskOwner                    -- programmer
          , Obj . verbLocation . activeFrame   -- verb-loc
          , Str . verbFullName . activeFrame   -- verb-name
          , Int . fromIntegral .
                  lineNumber   . activeFrame   -- line
          , Obj . initialThis  . activeFrame   -- this
          , Int . fromIntegral . taskSize      -- task-size
          ]

bf_kill_task = Builtin "kill_task" 1 (Just 1) [TInt] TAny $ \[Int task_id] -> do
  let task_id' = fromIntegral task_id
  maybeTask <- getTask task_id'

  case maybeTask of
    Just task@Task { taskStatus = status } | isQueued status -> do
      checkPermission (taskOwner task)
      purgeTask task
      delayIO $ killThread (taskThread task)
      return zero
    _ -> do
      thisTaskId <- taskId <$> asks task
      if task_id' == thisTaskId
        then interrupt Suicide
        else raise E_INVARG

bf_callers = Builtin "callers" 0 (Just 1) [TAny] TLst $ \optional -> do
  let [include_line_numbers] = booleanDefaults optional [False]

  Stack frames <- gets stack
  return $ formatFrames include_line_numbers (tail frames)

bf_task_stack = Builtin "task_stack" 1 (Just 2)
                [TInt, TAny] TLst $ \(Int task_id : optional) -> do
  let [include_line_numbers] = booleanDefaults optional [False]

  maybeTask <- getTask (fromIntegral task_id)
  case maybeTask of
    Just task@Task { taskStatus = Suspended{} } -> do
      checkPermission (taskOwner task)
      let Stack frames = stack $ taskState task
      return $ formatFrames include_line_numbers frames
    _ -> raise E_INVARG
