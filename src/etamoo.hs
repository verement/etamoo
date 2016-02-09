
{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Monad (foldM, unless, when)
import Data.Char (isDigit)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isNothing, fromJust)
import Data.Version (showVersion)
import System.Console.GetOpt (OptDescr(Option), ArgDescr(NoArg, ReqArg),
                              ArgOrder(Permute), getOpt, usageInfo)
import System.Environment (getArgs, getProgName)

import MOO.Network
import MOO.Server
import MOO.Version

main :: IO ()
main = parseArgs >>= run

run :: Options -> IO ()
run opts
  | optHelp      opts = putStr =<< usage
  | optVersion   opts = putStr versionDetails
  | optImport    opts = importDatabase (fromJust $ optInputDB  opts)
                                       (fromJust $ optOutputDB opts)
  | optExport    opts = exportDatabase (fromJust $ optInputDB  opts)
                                       (fromJust $ optOutputDB opts)
  | otherwise         = startServer (optLogFile opts)
                        (fromJust $ optInputDB opts)
                        (optCacheSize opts)
                        (optEmergency opts)
                        (optOutboundNetwork opts)
                        (const $ TCP (optBindAddress opts) (optPort opts))

versionDetails :: String
versionDetails = unlines [
    "EtaMOO " ++ showVersion version ++ ", using:"
  , "  " ++ lmdbVersion
  , "  " ++ pcreVersion
  , "  " ++ runtimeVersion
  , ""
  , "Build options:"
# ifdef MOO_64BIT_INTEGER
  , "  64-bit MOO integers"
# else
  , "  32-bit MOO integers"
# endif
# ifdef MOO_OUTBOUND_NETWORK
  , "  open_network_connection() enabled by default"
# else
  , "  open_network_connection() disabled by default"
# endif
  ]

data Options = Options {
    optImport          :: Bool
  , optExport          :: Bool
  , optHelp            :: Bool
  , optVersion         :: Bool
  , optEmergency       :: Bool
  , optLogFile         :: Maybe FilePath
  , optInputDB         :: Maybe FilePath
  , optOutputDB        :: Maybe FilePath
  , optOutboundNetwork :: Bool
  , optBindAddress     :: Maybe HostName
  , optPort            :: PortNumber
  , optPortSpecified   :: Bool
  , optCacheSize       :: Int
  }

defaultOptions = Options {
    optImport          = False
  , optExport          = False
  , optHelp            = False
  , optVersion         = False
  , optEmergency       = False
  , optLogFile         = Nothing
  , optInputDB         = Nothing
  , optOutputDB        = Nothing
# ifdef MOO_OUTBOUND_NETWORK
  , optOutboundNetwork = True
# else
  , optOutboundNetwork = False
# endif
  , optBindAddress     = Nothing
  , optPort            = 7777
  , optPortSpecified   = False
  , optCacheSize       = 10
  }

options :: [OptDescr (Options -> Options)]
options = [
    Option "e" ["emergency"]
      (NoArg (\opts -> opts { optEmergency = True }))
      "Emergency Wizard Mode"
  , Option "l" ["log-file"]
      (ReqArg (\path opts -> opts { optLogFile = Just path }) "FILE")
      "Log file"
  , Option "" ["enable-outbound-network"]
      (NoArg (\opts -> opts { optOutboundNetwork = True }))
      $ "Enable  open_network_connection()" ++
        if outboundNetwork then " *" else ""
  , Option "O" ["disable-outbound-network"]
      (NoArg (\opts -> opts { optOutboundNetwork = False }))
      $ "Disable open_network_connection()" ++
        if not outboundNetwork then " *" else ""
  , Option "a" ["bind-address"]
      (ReqArg (\ip opts -> opts { optBindAddress = Just ip }) "IP-ADDR")
      "Bind address for connections"
  , Option "p" ["port"]
      (ReqArg (\port opts -> opts { optPort = fromInteger $ read port
                                  , optPortSpecified = True }) "PORT")
      $ "Listening port (default: " ++ show (optPort defaultOptions) ++ ")"
  , Option "C" ["cache-size"]
      (ReqArg (\size opts -> opts { optCacheSize = read size }) "SIZE")
      $ "Cache size in megabytes (default: " ++
        show (optCacheSize defaultOptions) ++ ")"
  , Option "" ["import"]
      (NoArg (\opts -> opts { optImport = True }))
      "Import LambdaMOO-format database"
  , Option "" ["export"]
      (NoArg (\opts -> opts { optExport = True }))
      "Export LambdaMOO-format database"
  , Option "V" ["version"]
      (NoArg (\opts -> opts { optVersion = True }))
      "Show server version and build details"
  , Option "h?" ["help"]
      (NoArg (\opts -> opts { optHelp = True }))
      "Show this usage"
  ]
  where outboundNetwork = optOutboundNetwork defaultOptions

usage :: IO String
usage = do
  argv0 <- getProgName
  let header = init $ unlines [
          "Usage: " ++ argv0 ++ " [-e] [-l FILE] " ++
                                "ETAMOO-DB [+O|-O] [-a IP-ADDR] [[-p] PORT]"
        , "       " ++ argv0 ++ " --import LAMBDAMOO-DB ETAMOO-DB"
        , "       " ++ argv0 ++ " --export ETAMOO-DB LAMBDAMOO-DB"
        ]
  return $ patchUsage (usageInfo header options) ++
    unlines ((replicate 68 ' ' ++ "(* default)") : "" : rtsOptions)

  where patchUsage :: String -> String
        patchUsage = unlines . map patch . lines
          where patch str
                  | "--enable-outbound-network" `isInfixOf` str &&
                    "      " `isPrefixOf` str = take 2 str ++ "+O" ++ drop 4 str
                  | otherwise                 = str

        rtsOptions :: [String]
        rtsOptions = [
            "Run time system options (use between +RTS and -RTS):"
          , "  -N<n>  Use <n> processors for multithreading (default: all)"
          , "  -T     Enable statistics for memory_usage() built-in function"
          , "  -?     Show other run time system options"
          ]

usageError :: String -> IO a
usageError msg = error . (msg ++) . ("\n\n" ++) . init =<< usage

serverOpts :: IO (Options, [String])
serverOpts = do
  args <- getArgs
  case getOpt Permute options args of
    (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) -> usageError (init $ concat errs)

parseArgs :: IO Options
parseArgs = do
  (opts, nonOpts) <- serverOpts
  opts <- foldM handleArg opts nonOpts

  unless (optHelp opts || optVersion opts) $ do
    when (optImport opts && optExport opts) $ usageError "usage error"
    when (isNothing $ optInputDB opts) $ usageError "missing input DB"
    when (optImport opts || optExport opts) $
      when (isNothing $ optOutputDB opts) $ usageError "missing output DB"

  return opts

  where handleArg :: Options -> String -> IO Options
        handleArg opts arg = case arg of
          "+O"  -> return opts { optOutboundNetwork = True }
          '+':_ -> usageError $ "unrecognized option `" ++ arg ++ "'"
          _ | isNothing (optInputDB opts) ->
                return opts { optInputDB = Just arg }
            | isNothing (optOutputDB opts) &&
              (optImport opts || optExport opts) ->
                return opts { optOutputDB = Just arg }
            | not (optPortSpecified opts) && all isDigit arg ->
                return opts { optPort = fromInteger $ read arg
                            , optPortSpecified = True }
            | otherwise -> usageError $ "unknown argument `" ++ arg ++ "'"
