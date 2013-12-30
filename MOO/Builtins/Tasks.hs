
{-# LANGUAGE OverloadedStrings #-}

module MOO.Builtins.Tasks ( builtins ) where

import Control.Monad (liftM)
import Control.Monad.Reader (asks)
import Control.Monad.State (gets, modify)
import Control.Monad.Cont (callCC)

import MOO.Types
import MOO.Task
import MOO.Object
import MOO.Parser
import {-# SOURCE #-} MOO.Compiler
import {-# SOURCE #-} MOO.Builtins
import MOO.Builtins.Common

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- 4.4.6 MOO-Code Evaluation and Task Manipulation

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
  , ("suspend"       , (bf_suspend       , Info 0 (Just 1) [TInt]       TAny))
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
  Lst $ V.fromList
  [Str name, Int $ fromIntegral min, Int $ maybe (-1) fromIntegral max,
   Lst $ V.fromList $ map (Int . typeCode) types]

bf_function_info [] = return $ Lst $ V.fromList $
                      map formatInfo $ Map.assocs builtinFunctions
bf_function_info [Str name] =
  case Map.lookup name' builtinFunctions of
    Just detail -> return $ formatInfo (name', detail)
    Nothing     -> raise E_INVARG
  where name' = T.toCaseFold name

bf_eval [Str string] = do
  checkProgrammer
  case parse string of
    Left errors   -> return $ Lst $ V.fromList
                     [truthValue False,
                      Lst $ V.fromList $ map (Str . T.pack) errors]
    Right program -> do
      (programmer, this, player) <- frame $ \frame ->
        (permissions frame, initialThis frame, initialPlayer frame)
      value <- evalFromFunc "eval" 0 $
        runVerbFrame (compile program) initFrame {
            variables     = Map.insert "player" (Obj player) $
                            Map.insert "caller" (Obj this) $ variables initFrame
          , permissions   = programmer
          , initialPlayer = player
          }
      return $ Lst $ V.fromList [truthValue True, value]

bf_set_task_perms [Obj who] = do
  checkPermission who
  modifyFrame $ \frame -> frame { permissions = who }
  return nothing

bf_caller_perms [] = (Obj . objectForMaybe) `liftM` caller permissions

bf_ticks_left [] = (Int . fromIntegral) `liftM` gets ticksLeft

bf_seconds_left [] = return (Int 5)  -- XXX can this be measured?

bf_task_id [] = (Int . taskId) `liftM` asks task

bf_suspend optional = do
  value <- callCC $ interrupt . Suspend (fmap fromInt maybeSeconds) . Resume
  modify $ \st -> st { ticksLeft = 15000 }
  return value
  where (maybeSeconds : _) = maybeDefaults optional

bf_resume (Int task_id : optional) = notyet "resume"
  where [value] = defaults optional [nothing]

bf_queue_info [] = notyet "queue_info"
bf_queue_info [Obj player] = notyet "queue_info"

bf_queued_tasks [] = notyet "queued_tasks"
bf_kill_task [Int task_id] = notyet "kill_task"

bf_callers optional = do
  Stack frames <- gets stack
  return $ formatFrames include_line_numbers (tail frames)
  where [include_line_numbers] = booleanDefaults optional [False]

bf_task_stack (Int task_id : optional) = notyet "task_stack"
  where [include_line_numbers] = booleanDefaults optional [False]
