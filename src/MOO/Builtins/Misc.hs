
{-# LANGUAGE CPP, OverloadedStrings #-}

module MOO.Builtins.Misc (builtins) where

import Control.Applicative ((<$>))
import Control.Monad.State (gets)
import Data.Monoid ((<>))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Database.VCache (VCacheStats(..), vcacheStats)

# ifdef __GLASGOW_HASKELL__
import GHC.Stats (GCStats(currentBytesUsed, maxBytesUsed),
                  getGCStats, getGCStatsEnabled)
# endif

import MOO.Builtins.Common
import MOO.Database
import MOO.Object
import MOO.Task
import MOO.Types
import MOO.Util
import MOO.Version

import qualified MOO.String as Str

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- | § 4.4 Built-in Functions
builtins :: [Builtin]
builtins = [
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
    Just parent -> callVerb this parent name args
    Nothing     -> raise E_VERBNF

-- § 4.4.5 Operations Involving Times and Dates

currentTime :: MOO IntT
currentTime = floor . utcTimeToPOSIXSeconds <$> gets startTime

bf_time = Builtin "time" 0 (Just 0) [] TInt $ \[] -> Int <$> currentTime

bf_ctime = Builtin "ctime" 0 (Just 1) [TInt] TStr $ \arg -> case arg of
  []         -> ctime' =<< currentTime
  [Int time] -> ctime' time

  where ctime' :: IntT -> MOO Value
        ctime' time = do
          let utcTime = posixSecondsToUTCTime (fromIntegral time)
          Str . Str.fromString <$>
            ctime utcTime `catchUnsafeIOtoMOO` \_ -> raise E_INVARG

-- § 4.4.7 Administrative Operations

bf_dump_database = Builtin "dump_database" 0 (Just 0) [] TAny $ \[] ->
  checkWizard >> getWorld >>= liftSTM . checkpoint >> return zero

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

  (new, db) <- liftVTx . renumber object =<< getDatabase
  putDatabase db

  return (Obj new)

bf_reset_max_object = Builtin "reset_max_object" 0 (Just 0) [] TAny $ \[] -> do
  checkWizard
  getDatabase >>= liftVTx . resetMaxObject >>= putDatabase
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

bf_db_disk_size = Builtin "db_disk_size" 0 (Just 1)
                  [TAny] TAny $ \optional -> do
  let [full] = booleanDefaults optional [False]
  stats <- unsafeIOtoMOO . vcacheStats =<< getVSpace

  return $ if full
           then fromList $ map (keyValue stats) [
               ("file_size",    vcstat_file_size)
             , ("vref_count",   vcstat_vref_count)
             , ("pvar_count",   vcstat_pvar_count)
             , ("root_count",   vcstat_root_count)
             , ("mem_vrefs",    vcstat_mem_vrefs)
             , ("mem_pvars",    vcstat_mem_pvars)
             , ("eph_count",    vcstat_eph_count)
             , ("alloc_count",  vcstat_alloc_count)
             , ("cache_limit",  vcstat_cache_limit)
             , ("cache_size",   vcstat_cache_size)
             , ("gc_count",     vcstat_gc_count)
             , ("write_pvars",  vcstat_write_pvars)
             , ("write_sync",   vcstat_write_sync)
             , ("write_frames", vcstat_write_frames)
             ]
           else Int $ fromIntegral (vcstat_file_size stats)

  where keyValue :: VCacheStats -> (StrT, VCacheStats -> Int) -> Value
        keyValue stats (key, f) =
          fromList [Str key, Int . fromIntegral $ f stats]

bf_verb_cache_stats = Builtin "verb_cache_stats" 0 (Just 0) [] TLst $ \[] ->
  notyet "verb_cache_stats"
bf_log_cache_stats  = Builtin "log_cache_stats"  0 (Just 0) [] TAny $ \[] ->
  notyet "log_cache_stats"
