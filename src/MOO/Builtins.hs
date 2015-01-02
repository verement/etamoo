
{-# LANGUAGE OverloadedStrings #-}

module MOO.Builtins ( builtinFunctions, callBuiltin, verifyBuiltins ) where

import Control.Monad (when, foldM, liftM, join)
import Control.Monad.State (gets)
import Data.List (transpose, inits)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Time (formatTime, utcToLocalZonedTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import System.Locale (defaultTimeLocale)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

import MOO.Builtins.Common
import MOO.Types
import MOO.Task
import MOO.Database
import MOO.Object
import MOO.Version

import MOO.Builtins.Values  as Values
import MOO.Builtins.Objects as Objects
import MOO.Builtins.Network as Network
import MOO.Builtins.Tasks   as Tasks

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- | A 'Map' of all built-in functions, keyed by name
builtinFunctions :: Map Id Builtin
builtinFunctions =
  M.fromList $ map assoc $ miscBuiltins ++
  Values.builtins ++ Objects.builtins ++ Network.builtins ++ Tasks.builtins
  where assoc builtin = (builtinName builtin, builtin)

-- | Call the named built-in function with the given arguments, checking first
-- for the appropriate number and types of arguments. Raise 'E_INVARG' if the
-- built-in function is unknown.
callBuiltin :: Id -> [Value] -> MOO Value
callBuiltin func args = case M.lookup func builtinFunctions of
  Just builtin -> checkArgs builtin args >> builtinFunction builtin args
  Nothing      -> raiseException (Err E_INVARG)
                  "Unknown built-in function" (Str func)

  where checkArgs Builtin {
            builtinMinArgs  = min
          , builtinMaxArgs  = max
          , builtinArgTypes = types
          } args = do
          let nargs = length args
            in when (nargs < min || nargs > fromMaybe nargs max) $ raise E_ARGS
          checkTypes types args

        checkTypes (t:ts) (a:as) = do
          when (typeMismatch t $ typeOf a) $ raise E_TYPE
          checkTypes ts as
        checkTypes _ _ = return ()

        typeMismatch x    y    | x == y = False
        typeMismatch TAny _             = False
        typeMismatch TNum TInt          = False
        typeMismatch TNum TFlt          = False
        typeMismatch _    _             = True

-- | Perform internal consistency verification of all the built-in functions,
-- checking that each implementation actually accepts the claimed argument
-- types. Note that an inconsistency may cause the program to abort.
--
-- Assuming the program doesn't abort, this generates either a string
-- describing an inconsistency, or an integer giving the total number of
-- (verified) built-in functions.
verifyBuiltins :: Either String Int
verifyBuiltins = foldM accum 0 $ M.elems builtinFunctions
  where accum a b = valid b >>= Right . (+ a)
        valid Builtin {
            builtinName     = name
          , builtinMinArgs  = min
          , builtinMaxArgs  = max
          , builtinArgTypes = types
          , builtinFunction = func
          }
          | name /= T.toCaseFold name         = invalid "name not case-folded"
          | min < 0                           = invalid "arg min < 0"
          | maybe False (< min) max           = invalid "arg max < min"
          | length types /= fromMaybe min max = invalid "incorrect # types"
          | testArgs func min max types       = ok
          where invalid msg = Left $ T.unpack name ++ ": " ++ msg
                ok = Right 1

        testArgs func min max types = all test argSpecs
          where argSpecs = drop min $ inits $ map mkArgs augmentedTypes
                augmentedTypes = maybe (types ++ [TAny]) (const types) max
                test argSpec = all (\args -> func args `seq` True) $
                               enumerateArgs argSpec

        enumerateArgs :: [[Value]] -> [[Value]]
        enumerateArgs (a:[]) = transpose [a]
        enumerateArgs (a:as) = concatMap (combine a) (enumerateArgs as)
          where combine ps rs = map (: rs) ps
        enumerateArgs []     = [[]]

        mkArgs :: Type -> [Value]
        mkArgs TAny = mkArgs TNum ++ mkArgs TStr ++ mkArgs TObj ++
                      mkArgs TErr ++ mkArgs TLst
        mkArgs TNum = mkArgs TInt ++ mkArgs TFlt
        mkArgs TInt = [Int 0]
        mkArgs TFlt = [Flt 0]
        mkArgs TStr = [Str T.empty]
        mkArgs TObj = [Obj 0]
        mkArgs TErr = [Err E_NONE]
        mkArgs TLst = [Lst V.empty]

-- § 4.4 Built-in Functions

miscBuiltins :: [Builtin]
miscBuiltins = [
    -- § 4.4.1 Object-Oriented Programming
    bf_pass

    -- § 4.4.5 Operations Involving Times and Dates
  , bf_time
  , bf_ctime

    -- § 4.4.7 Administrative Operations
  , bf_dump_database
  , bf_shutdown
  , bf_load_server_options
  , bf_server_log
  , bf_renumber
  , bf_reset_max_object

    -- § 4.4.8 Server Statistics and Miscellaneous Information
  , bf_server_version
  , bf_memory_usage
  , bf_db_disk_size
  , bf_verb_cache_stats
  , bf_log_cache_stats
  ]

-- § 4.4.1 Object-Oriented Programming

bf_pass = Builtin "pass" 0 Nothing [] TAny $ \args -> do
  (name, verbLoc, this) <- frame $ \frame ->
    (verbName frame, verbLocation frame, initialThis frame)
  maybeMaybeParent <- fmap objectParent `liftM` getObject verbLoc
  case join maybeMaybeParent of
    Just parent -> callVerb parent this name args
    Nothing     -> raise E_VERBNF

-- § 4.4.5 Operations Involving Times and Dates

currentTime :: MOO IntT
currentTime = (floor . utcTimeToPOSIXSeconds) `liftM` gets startTime

bf_time = Builtin "time" 0 (Just 0) [] TInt $ \[] -> Int `liftM` currentTime

bf_ctime = Builtin "ctime" 0 (Just 1) [TInt] TStr go
  where go []         = ctime =<< currentTime
        go [Int time] = ctime time

ctime :: IntT -> MOO Value
ctime time = do
  zonedTime <- unsafeIOtoMOO (utcToLocalZonedTime utcTime)
  return $ Str $ T.pack $ formatTime defaultTimeLocale format zonedTime
  where utcTime = posixSecondsToUTCTime (fromIntegral time)
        format  = "%a %b %_d %T %Y %Z"

-- § 4.4.7 Administrative Operations

bf_dump_database = Builtin "dump_database" 0 (Just 0) [] TAny $ \[] ->
  notyet "dump_database"

bf_shutdown = Builtin "shutdown" 0 (Just 1) [TStr] TAny go
  where go optional = notyet "shutdown"
          where (message : _) = maybeDefaults optional

bf_load_server_options = Builtin "load_server_options" 0 (Just 0) [] TAny go
  where go [] = checkWizard >> loadServerOptions >> return nothing

bf_server_log = Builtin "server_log" 1 (Just 2) [TStr, TAny] TAny go
  where go (Str message : optional) = notyet "server_log"
          where [is_error] = booleanDefaults optional [False]

bf_renumber = Builtin "renumber" 1 (Just 1) [TObj] TObj $ \[Obj object] ->
  notyet "renumber"

bf_reset_max_object = Builtin "reset_max_object" 0 (Just 0) [] TAny $ \[] -> do
  checkWizard
  getDatabase >>= liftSTM . resetMaxObject >>= putDatabase
  return nothing

-- § 4.4.8 Server Statistics and Miscellaneous Information

bf_server_version = Builtin "server_version" 0 (Just 0) [] TStr $ \[] ->
  return (Str serverVersionText)

bf_memory_usage = Builtin "memory_usage" 0 (Just 0) [] TLst $ \[] ->
  return (Lst V.empty)  -- ... nothing to see here

bf_db_disk_size = Builtin "db_disk_size" 0 (Just 0) [] TInt $ \[] ->
  raise E_QUOTA  -- not yet?

bf_verb_cache_stats = Builtin "verb_cache_stats" 0 (Just 0) [] TLst $ \[] ->
  notyet "verb_cache_stats"
bf_log_cache_stats  = Builtin "log_cache_stats"  0 (Just 0) [] TAny $ \[] ->
  notyet "log_cache_stats"
