
{-# LANGUAGE OverloadedStrings #-}

module MOO.Builtins.Task ( builtins ) where

import MOO.Types
import MOO.Execution
import {-# SOURCE #-} MOO.Builtins
import MOO.Builtins.Common

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V

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

bf_raise [code] =
  raiseException $ Exception code (toText code) (Int 0)
bf_raise [code, Str message] =
  raiseException $ Exception code message (Int 0)
bf_raise [code, Str message, value] =
  raiseException $ Exception code message value

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
  case Map.lookupIndex (T.toCaseFold name) builtinFunctions of
    Just index -> return $ formatInfo $ Map.elemAt index builtinFunctions
    Nothing    -> raise E_INVARG

bf_eval [Str string] = notyet

bf_set_task_perms [Obj who] = notyet

bf_caller_perms [] = notyet

bf_ticks_left [] = notyet
bf_seconds_left [] = notyet

bf_task_id [] = notyet

bf_suspend seconds = notyet
bf_resume (Int task_id : value) = notyet

bf_queue_info player = notyet
bf_queued_tasks [] = notyet
bf_kill_task [Int task_id] = notyet

bf_callers include_line_numbers = notyet

bf_task_stack (Int task_id : include_line_numbers) = notyet
