
{-# LANGUAGE OverloadedStrings #-}

module MOO.Builtins.Tasks ( builtins ) where

import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.STM
import Control.Monad (liftM, void)
import Control.Monad.Cont (callCC)
import Control.Monad.Reader (asks)
import Control.Monad.State (gets, modify, get)
import Data.List (sort)
import Data.Time (getCurrentTime, addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Posix (nanosleep)

import MOO.Types
import MOO.Task
import MOO.Object
import MOO.Verb
import MOO.Parser
import {-# SOURCE #-} MOO.Compiler
import {-# SOURCE #-} MOO.Builtins
import MOO.Builtins.Common

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- | § 4.4.6 MOO-Code Evaluation and Task Manipulation
builtins :: [BuiltinSpec]
builtins = [
    ("raise"         , (bf_raise         , Info 1 (Just 3) [TAny, TStr,
                                                            TAny]       TAny))
  , ("call_function" , (bf_call_function , Info 1 Nothing  [TStr]       TAny))
  , ("function_info" , (bf_function_info , Info 0 (Just 1) [TStr]       TLst))
  , ("eval"          , (bf_eval          , Info 1 (Just 1) [TStr]       TLst))
  , ("set_task_perms", (bf_set_task_perms, Info 1 (Just 1) [TObj]       TAny))
  , ("caller_perms"  , (bf_caller_perms  , Info 0 (Just 0) []           TObj))
  , ("ticks_left"    , (bf_ticks_left    , Info 0 (Just 0) []           TInt))
  , ("seconds_left"  , (bf_seconds_left  , Info 0 (Just 0) []           TInt))
  , ("task_id"       , (bf_task_id       , Info 0 (Just 0) []           TInt))
  , ("suspend"       , (bf_suspend       , Info 0 (Just 1) [TNum]       TAny))
  , ("resume"        , (bf_resume        , Info 1 (Just 2) [TInt, TAny] TAny))
  , ("queue_info"    , (bf_queue_info    , Info 0 (Just 1) [TObj]       TLst))
  , ("queued_tasks"  , (bf_queued_tasks  , Info 0 (Just 0) []           TLst))
  , ("kill_task"     , (bf_kill_task     , Info 1 (Just 1) [TInt]       TAny))
  , ("callers"       , (bf_callers       , Info 0 (Just 1) [TAny]       TLst))
  , ("task_stack"    , (bf_task_stack    , Info 1 (Just 2) [TInt, TAny] TLst))
  ]

bf_raise (code : optional) = raiseException $ Exception code message value
  where [Str message, value] = defaults optional [Str $ toText code, nothing]

bf_call_function (Str func_name : args) =
  callBuiltin (T.toCaseFold func_name) args

formatInfo :: (Id, (Builtin, Info)) -> Value
formatInfo (name, (_, Info min max types _)) =
  fromList [ Str name
           , Int $ fromIntegral min
           , Int $ maybe (-1) fromIntegral max
           , fromListBy (Int . typeCode) types
           ]

bf_function_info [] = return $ fromListBy formatInfo $ M.assocs builtinFunctions
bf_function_info [Str name] =
  case M.lookup name' builtinFunctions of
    Just detail -> return $ formatInfo (name', detail)
    Nothing     -> raise E_INVARG
  where name' = T.toCaseFold name

bf_eval [Str string] = do
  checkProgrammer
  case parse string of
    Left errors   -> return $ fromList [truthValue False,
                                        fromListBy (Str . T.pack) errors]
    Right program -> do
      (programmer, this, player) <- frame $ \frame ->
        (permissions frame, initialThis frame, initialPlayer frame)
      let verb = initVerb {
              verbNames   = "Input to EVAL"
            , verbProgram = program
            , verbCode    = compile program
            , verbOwner   = programmer
            , verbPermD   = True
            }
          vars = mkVariables [
              ("player", Obj player)
            , ("caller", Obj this)
            ]
      value <- evalFromFunc "eval" 0 $
        runVerb verb initFrame {
            variables     = vars
          , initialPlayer = player
          }
      return $ fromList [truthValue True, value]

bf_set_task_perms [Obj who] = do
  checkPermission who
  modifyFrame $ \frame -> frame { permissions = who }
  return nothing

bf_caller_perms [] = (Obj . objectForMaybe) `liftM` caller permissions

bf_ticks_left [] = (Int . fromIntegral) `liftM` gets ticksLeft

bf_seconds_left [] = return (Int 5)  -- XXX can this be measured?

bf_task_id [] = (Int . fromIntegral) `liftM` asks (taskId . task)

bf_suspend optional = do
  maybeMicroseconds <- case maybeSeconds of
    Just (Int secs)
      | secs < 0  -> raise E_INVARG
      | otherwise -> return (Just $ fromIntegral secs * 1000000)
    Just (Flt secs)
      | secs < 0  -> raise E_INVARG
      | otherwise -> return (Just $ ceiling    $ secs * 1000000)
    Nothing -> return Nothing

  state <- get

  estimatedWakeup <- case maybeMicroseconds of
    Just usecs
      | time < now || time > endOfTime -> raise E_INVARG
      | otherwise                      -> return time
      where now = startTime state
            time = (fromIntegral usecs / 1000000) `addUTCTime` now

    Nothing -> return endOfTime  -- XXX this is a sad wart in need of remedy

  resumeTMVar <- liftSTM newEmptyTMVar
  task <- asks task

  let wake value = do
        now <- getCurrentTime
        atomically $ putTMVar resumeTMVar (now, value)

      task' = task {
          taskStatus = Suspended (Wake wake)
        , taskState  = state { startTime = estimatedWakeup }
        }

  case maybeMicroseconds of
    Just usecs -> delayIO $ void $ forkIO $ do
      if usecs <= fromIntegral (maxBound :: Int)
        then threadDelay (fromIntegral usecs)
        else nanosleep (usecs * 1000)
      wake nothing
    Nothing -> return ()

  putTask task'

  callCC $ interrupt . Suspend maybeMicroseconds . Resume
  (now, value) <- liftSTM $ takeTMVar resumeTMVar

  putTask task' { taskStatus = Running }

  modify $ \state -> state { ticksLeft = 15000, startTime = now }  -- XXX ticks

  case value of
    Err error -> raise error
    _         -> return value

  where (maybeSeconds : _) = maybeDefaults optional

bf_resume (Int task_id : optional) = do
  maybeTask <- getTask task_id'
  case maybeTask of
    Just task@Task { taskStatus = Suspended (Wake wake) } -> do
      checkPermission (taskOwner task)
      putTask task { taskStatus = Running }
      delayIO (wake value)
    _ -> raise E_INVARG

  return nothing

  where [value] = defaults optional [nothing]
        task_id' = fromIntegral task_id

bf_queue_info [] =
  (objectList . S.toList . foldr (S.insert . taskOwner) S.empty) `liftM`
  queuedTasks

bf_queue_info [Obj player] =
  (Int . fromIntegral . length . filter ((== player) . taskOwner)) `liftM`
  queuedTasks

bf_queued_tasks [] = do
  tasks <- queuedTasks
  programmer <- frame permissions
  wizard <- isWizard programmer
  let ownedTasks = if wizard then tasks
                   else filter ((== programmer) . taskOwner) tasks

  return $ fromListBy formatTask $ sort ownedTasks

  where formatTask task = fromListBy ($ task) [
            Int . fromIntegral . taskId        -- task-id
          , Int . floor . utcTimeToPOSIXSeconds . startTime . taskState
                                               -- start-time
          , const (Int 0)                      -- clock-id
          , const (Int 15000)                  -- ticks XXX
          , Obj . taskOwner                    -- programmer
          , Obj . verbLocation . activeFrame   -- verb-loc
          , Str . verbFullName . activeFrame   -- verb-name
          , Int . lineNumber   . activeFrame   -- line
          , Obj . initialThis  . activeFrame   -- this
          , Int . fromIntegral . storageBytes  -- task-size
          ]

bf_kill_task [Int task_id] = do
  maybeTask <- getTask task_id'
  case maybeTask of
    Just task@Task { taskStatus = status } | isQueued status -> do
      checkPermission (taskOwner task)
      purgeTask task
      delayIO $ killThread (taskThread task)
      return nothing
    _ -> do
      thisTaskId <- taskId `liftM` asks task
      if task_id' == thisTaskId
        then interrupt Suicide
        else raise E_INVARG

  where task_id' = fromIntegral task_id

bf_callers optional = do
  Stack frames <- gets stack
  return $ formatFrames include_line_numbers (tail frames)

  where [include_line_numbers] = booleanDefaults optional [False]

bf_task_stack (Int task_id : optional) = do
  maybeTask <- getTask task_id'
  case maybeTask of
    Just task@Task { taskStatus = Suspended{} } -> do
      checkPermission (taskOwner task)
      let Stack frames = stack $ taskState task
      return $ formatFrames include_line_numbers frames
    _ -> raise E_INVARG

  where [include_line_numbers] = booleanDefaults optional [False]
        task_id' = fromIntegral task_id
