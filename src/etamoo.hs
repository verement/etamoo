
{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Monad (foldM, unless)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing, fromJust)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..),
                              getOpt, usageInfo)
import System.Environment (getArgs, getProgName)

import MOO.Network
import MOO.Server

main :: IO ()
main = parseArgs >>= run

run :: Options -> IO ()
run opts
  | optHelp opts      = putStrLn =<< usage
  | optEmergency opts = error "Emergency Wizard Mode not yet implemented"
  | otherwise         = startServer (optLogFile opts)
                        (fromJust $ optInputDB opts)
                        (fromJust $ optOutputDB opts)
                        (optOutboundNetwork opts)
                        (const $ TCP (optBindAddress opts) (optPort opts))

data Options = Options {
    optHelp            :: Bool
  , optEmergency       :: Bool
  , optLogFile         :: Maybe FilePath
  , optInputDB         :: Maybe FilePath
  , optOutputDB        :: Maybe FilePath
  , optOutboundNetwork :: Bool
  , optBindAddress     :: Maybe HostName
  , optPort            :: PortNumber
  , optPortSpecified   :: Bool
  }

defaultOptions = Options {
    optHelp            = False
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
      (ReqArg (\port opts -> opts { optPort = fromInteger $ read port,
                                    optPortSpecified = True }) "PORT")
      $ "Listening port (default: " ++ show (optPort defaultOptions) ++ ")"
  , Option "h?" ["help"]
      (NoArg (\opts -> opts { optHelp = True }))
      "Show this usage"
  ]
  where outboundNetwork = optOutboundNetwork defaultOptions

usage :: IO String
usage = do
  argv0 <- getProgName
  let header = "Usage: " ++ argv0 ++ " [-e] [-l FILE] " ++
               "INPUT-DB OUTPUT-DB [+O|-O] [-a IP-ADDR] [[-p] PORT]"
  return $ patchUsage (usageInfo header options) ++
    "* Default outbound network option"

  where patchUsage :: String -> String
        patchUsage = unlines . map patch . lines
          where patch str
                  | "--enable-outbound-network" `isInfixOf` str &&
                    "      " `isPrefixOf` str = take 2 str ++ "+O" ++ drop 4 str
                  | otherwise                 = str

usageError :: String -> IO a
usageError msg = usage >>= error . ((msg ++ "\n") ++)

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

  unless (optHelp opts) $ do
    unless (isJust $ optInputDB  opts) $ usageError "missing INPUT-DB"
    unless (isJust $ optOutputDB opts) $ usageError "missing OUTPUT-DB"

  return opts

  where handleArg :: Options -> String -> IO Options
        handleArg opts arg = case arg of
          "+O"  -> return opts { optOutboundNetwork = True }
          '+':_ -> usageError $ "unrecognized option `" ++ arg ++ "'"
          _ | isNothing (optInputDB opts) ->
                return opts { optInputDB = Just arg }
            | isNothing (optOutputDB opts) ->
                return opts { optOutputDB = Just arg }
            | not (optPortSpecified opts) ->
                return opts { optPort = fromInteger $ read arg,
                              optPortSpecified = True }
            | otherwise -> usageError $ "unknown argument `" ++ arg ++ "'"
