
{-# LANGUAGE CPP, OverloadedStrings #-}

module MOO.Builtins ( builtinFunctions, callBuiltin, verifyBuiltins ) where

import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Control.Monad.State (gets)
import Data.HashMap.Lazy (HashMap)
import Data.List (transpose, inits)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Time (formatTime, utcToLocalZonedTime, defaultTimeLocale)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)

# ifdef __GLASGOW_HASKELL__
import GHC.Stats (GCStats(currentBytesUsed, maxBytesUsed),
                  getGCStats, getGCStatsEnabled)
# endif

import qualified Data.HashMap.Lazy as HM

import MOO.Builtins.Common
import MOO.Database
import MOO.Object
import MOO.Task
import MOO.Types
import MOO.Version

import MOO.Builtins.Extra   as Extra
import MOO.Builtins.Network as Network
import MOO.Builtins.Objects as Objects
import MOO.Builtins.Tasks   as Tasks
import MOO.Builtins.Values  as Values

import qualified MOO.String as Str

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- | A 'HashMap' of all built-in functions, keyed by name
builtinFunctions :: HashMap Id Builtin
builtinFunctions =
  HM.fromList $ map assoc $ Extra.builtins ++ miscBuiltins ++
  Values.builtins ++ Objects.builtins ++ Network.builtins ++ Tasks.builtins
  where assoc builtin = (builtinName builtin, builtin)

-- | Call the named built-in function with the given arguments, checking first
-- for the appropriate number and types of arguments. Raise 'E_INVARG' if the
-- built-in function is unknown.
callBuiltin :: Id -> [Value] -> MOO Value
callBuiltin func args = do
  isProtected <- ($ func) <$> serverOption protectFunction
  case (func `HM.lookup` builtinFunctions, isProtected) of
    (Just builtin, False) -> call builtin
    (Just builtin, True)  -> do
      this <- frame initialThis
      if this == systemObject then call builtin
        else callSystemVerb ("bf_" <> fromId func) args >>=
             maybe (checkWizard >> call builtin) return
    (Nothing, _) -> let name = fromId func
                        message = "Unknown built-in function: " <> name
                    in raiseException (Err E_INVARG) message (Str name)

  where call :: Builtin -> MOO Value
        call builtin = checkArgs builtin args >> builtinFunction builtin args

        checkArgs :: Builtin -> [Value] -> MOO ()
        checkArgs Builtin { builtinMinArgs  = min
                          , builtinMaxArgs  = max
                          , builtinArgTypes = types
                          } args
          | nargs < min || nargs > fromMaybe nargs max = raise E_ARGS
          | otherwise                                  = checkTypes types args
          where nargs = length args

        checkTypes :: [Type] -> [Value] -> MOO ()
        checkTypes (t:ts) (v:vs)
          | typeMismatch t (typeOf v) = raise E_TYPE
          | otherwise                 = checkTypes ts vs
        checkTypes _ _ = return ()

        typeMismatch :: Type -> Type -> Bool
        typeMismatch a    b    | a == b = False
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
verifyBuiltins = foldM accum 0 $ HM.elems builtinFunctions

  where accum :: Int -> Builtin -> Either String Int
        accum a b = valid b >>= Right . (+ a)

        valid :: Builtin -> Either String Int
        valid Builtin { builtinName     = name
                      , builtinMinArgs  = min
                      , builtinMaxArgs  = max
                      , builtinArgTypes = types
                      , builtinFunction = func
                      }
          | min < 0                           = invalid "arg min < 0"
          | maybe False (< min) max           = invalid "arg max < min"
          | length types /= fromMaybe min max = invalid "incorrect # types"
          | testArgs func min max types       = ok
          where invalid :: String -> Either String Int
                invalid msg = Left $ "problem with built-in function " ++
                              fromId name ++ ": " ++ msg
                ok = Right 1

        testArgs :: ([Value] -> MOO Value) -> Int -> Maybe Int -> [Type] -> Bool
        testArgs func min max types = all test argSpecs
          where argSpecs = drop min $ inits $ map mkArgs augmentedTypes
                augmentedTypes = maybe (types ++ [TAny]) (const types) max
                test argSpec = all (\args -> func args `seq` True) $
                               enumerateArgs argSpec

        enumerateArgs :: [[Value]] -> [[Value]]
        enumerateArgs [a]    = transpose [a]
        enumerateArgs (a:as) = concatMap (combine a) (enumerateArgs as)
          where combine ps rs = map (: rs) ps
        enumerateArgs []     = [[]]

        mkArgs :: Type -> [Value]
        mkArgs TAny = mkArgs TNum ++ mkArgs TStr ++ mkArgs TObj ++
                      mkArgs TErr ++ mkArgs TLst
        mkArgs TNum = mkArgs TInt ++ mkArgs TFlt
        mkArgs TInt = [Int 0]
        mkArgs TFlt = [Flt 0]
        mkArgs TStr = [emptyString]
        mkArgs TObj = [Obj 0]
        mkArgs TErr = [Err E_NONE]
        mkArgs TLst = [emptyList]

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
  maybeObject <- getObject verbLoc
  case maybeObject >>= objectParent of
    Just parent -> callVerb parent this name args
    Nothing     -> raise E_VERBNF

-- § 4.4.5 Operations Involving Times and Dates

currentTime :: MOO IntT
currentTime = floor . utcTimeToPOSIXSeconds <$> gets startTime

bf_time = Builtin "time" 0 (Just 0) [] TInt $ \[] -> Int <$> currentTime

bf_ctime = Builtin "ctime" 0 (Just 1) [TInt] TStr $ \arg -> case arg of
  []         -> ctime =<< currentTime
  [Int time] -> ctime time

ctime :: IntT -> MOO Value
ctime time = do
  zonedTime <- utcToLocalZonedTime utcTime `catchUnsafeIOtoMOO` \_ ->
    raise E_INVARG
  return $ Str $ Str.fromString $ formatTime defaultTimeLocale format zonedTime
  where utcTime = posixSecondsToUTCTime (fromIntegral time)
        format  = "%a %b %_d %T %Y %Z"

-- § 4.4.7 Administrative Operations

bf_dump_database = Builtin "dump_database" 0 (Just 0) [] TAny $ \[] ->
  notyet "dump_database"

bf_shutdown = Builtin "shutdown" 0 (Just 1) [TStr] TAny $ \optional -> do
  let (message : _) = maybeDefaults optional

  checkWizard

  name <- getObjectName =<< frame permissions
  let msg = "shutdown() called by " <> name

  shutdown $ maybe msg (\(Str reason) -> msg <> ": " <> reason) message
  return zero

bf_load_server_options = Builtin "load_server_options" 0 (Just 0)
                         [] TAny $ \[] ->
  checkWizard >> loadServerOptions >> return zero

bf_server_log = Builtin "server_log" 1 (Just 2)
                [TStr, TAny] TAny $ \(Str message : optional) -> do
  let [is_error]  = booleanDefaults optional [False]
      errorMarker = if is_error then "*** " else ""
      logMessage  = errorMarker <> "> " <> Str.toText message

  checkWizard

  world <- getWorld
  liftSTM $ writeLog world logMessage

  return zero

bf_renumber = Builtin "renumber" 1 (Just 1) [TObj] TObj $ \[Obj object] -> do
  checkValid object
  checkWizard

  (new, db) <- liftSTM . renumber object =<< getDatabase
  putDatabase db

  return (Obj new)

bf_reset_max_object = Builtin "reset_max_object" 0 (Just 0) [] TAny $ \[] -> do
  checkWizard
  getDatabase >>= liftSTM . resetMaxObject >>= putDatabase
  return zero

-- § 4.4.8 Server Statistics and Miscellaneous Information

bf_server_version = Builtin "server_version" 0 (Just 0) [] TStr $ \[] ->
  return (Str $ Str.fromText serverVersion)

bf_memory_usage = Builtin "memory_usage" 0 (Just 0) [] TLst $ \[] ->
# ifdef __GLASGOW_HASKELL__
  -- Server must be run with +RTS -T to enable statistics
  do maybeStats <- requestIO $ do
       enabled <- getGCStatsEnabled
       if enabled then Just <$> getGCStats else return Nothing

     return $ case maybeStats of
       Just stats ->
         let nused = currentBytesUsed stats
             nfree = maxBytesUsed stats - nused
             maxBlockSize = 2 ^ (floor $ logBase (2 :: Double) $
                                 fromIntegral $ max nused nfree :: Int)
         in fromListBy (fromListBy $ Int . fromIntegral) $
            blocks maxBlockSize nused nfree
       Nothing -> emptyList

       where blocks :: (Integral a) => a -> a -> a -> [[a]]
             blocks _         0     0     = []
             blocks blockSize nused nfree =
               let nusedBlocks = nused `div` blockSize
                   nfreeBlocks = nfree `div` blockSize
                   rest = blocks (blockSize `div` 2)
                          (nused - nusedBlocks * blockSize)
                          (nfree - nfreeBlocks * blockSize)
               in case (nusedBlocks, nfreeBlocks) of
                 (0, 0) -> rest
                 _      -> [blockSize, nusedBlocks, nfreeBlocks] : rest
# else
  return emptyList  -- ... nothing to see here
# endif

bf_db_disk_size = Builtin "db_disk_size" 0 (Just 0) [] TInt $ \[] ->
  notyet "db_disk_size"
  -- raise E_QUOTA

bf_verb_cache_stats = Builtin "verb_cache_stats" 0 (Just 0) [] TLst $ \[] ->
  notyet "verb_cache_stats"
bf_log_cache_stats  = Builtin "log_cache_stats"  0 (Just 0) [] TAny $ \[] ->
  notyet "log_cache_stats"
