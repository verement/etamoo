
module Main where

import System.Console.Readline
import System.Random
import System.Environment
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Control.Concurrent.STM
import Data.Text
import Data.Monoid (mempty)
import Data.Maybe

import MOO.Parser
import MOO.Compiler
import MOO.Task
import MOO.Types
import MOO.Builtins
import MOO.Database
import MOO.Database.LambdaMOO
import MOO.Object

main :: IO ()
main = do
  case verifyBuiltins of
    Left  err -> putStrLn $ "Built-in function verification failed: " ++ err
    Right n   -> do
      putStrLn $ show n ++ " built-in functions verified"
      db <- replDatabase
      tvarDB <- newTVarIO db
      gen <- getStdGen
      testFrame <- atomically $ mkTestFrame db
      repLoop $ addFrame testFrame (initState tvarDB gen)

replDatabase :: IO Database
replDatabase = do
  args <- getArgs
  case args of
    [dbFile] -> loadLMDatabase dbFile >>= either (error . show) return
    []       -> return initDatabase

repLoop state = do
  maybeLine <- readline ">> "
  case maybeLine of
    Nothing   -> return ()
    Just line -> do
      addHistory line
      run line state >>= repLoop

addFrame frame st@State { stack = Stack frames } =
  st { stack = Stack (frame : frames) }

mkTestFrame :: Database -> STM StackFrame
mkTestFrame db = do
  wizards <- filterM isWizard $ allPlayers db
  return $ initFrame True $ fromMaybe (-1) $ listToMaybe wizards
  where isWizard oid = fmap (maybe False objectWizard) $ dbObject oid db

alterFrame :: TaskState -> (StackFrame -> StackFrame) -> TaskState
alterFrame st@State { stack = Stack (frame:stack) } f =
  st { stack = Stack (f frame : stack) }

run ":+d" state = return $ alterFrame state $
                  \frame -> frame { debugBit = True  }
run ":-d" state = return $ alterFrame state $
                  \frame -> frame { debugBit = False }

run (':':'p':'e':'r':'m':' ':perm) state =
  return $ alterFrame state $ \frame -> frame { permissions = read perm }

run ":stack" state = print (stack state) >> return state

run line state = case runParser expression initParserState "" (pack line) of
  Left err -> putStr "Parse error " >> print err >> return state
  Right expr -> do
    putStrLn $ "-- " ++ show expr
    env <- initEnvironment
    let comp = compileExpr expr `catchException` \(Exception code m v) -> do
          delayIO $ putStrLn $ "** " ++ unpack m ++ formatValue v
          return code
        formatValue (Int 0) = ""
        formatValue v = " [" ++ unpack (toLiteral v) ++ "]"
        contM  = runReaderT comp env
        stateM = runContT contM return
        stmM   = runStateT stateM state { delayedIO = mempty }
    (value, state') <- atomically stmM
    runDelayed $ delayedIO state'
    putStrLn $ "=> " ++ unpack (toLiteral value)
    return state'
