
module Main where

import System.Console.Readline
import System.Random
import System.Environment
import Control.Monad.Reader
import Control.Concurrent.STM
import Data.Text
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
      repLoop tvarDB $ addFrame testFrame (initState gen)

replDatabase :: IO Database
replDatabase = do
  args <- getArgs
  case args of
    [dbFile] -> loadLMDatabase dbFile >>= either (error . show) return
    []       -> return initDatabase

repLoop db state = do
  maybeLine <- readline ">> "
  case maybeLine of
    Nothing   -> return ()
    Just line -> do
      addHistory line
      run db line state >>= repLoop db

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

run _ ":+d" state = return $ alterFrame state $
                  \frame -> frame { debugBit = True  }
run _ ":-d" state = return $ alterFrame state $
                  \frame -> frame { debugBit = False }

run _ (':':'p':'e':'r':'m':' ':perm) state =
  return $ alterFrame state $ \frame -> frame { permissions = read perm }

run _ ":stack" state = print (stack state) >> return state

run db line state =
  case runParser expression initParserState "" (pack line) of
    Left err -> putStr "Parse error " >> print err >> return state
    Right expr -> do
      putStrLn $ "-- " ++ show expr
      let comp = compileExpr expr
          comp' = fmap Complete comp
      task <- initTask comp'
      evalPrint db task state

evalPrint db task state = do
  (result, state') <- runTask db task state
  case result of
    Complete value -> do
      putStrLn $ "=> " ++ unpack (toLiteral value)
      return state'
    Suspend Nothing _  -> do
      putStrLn   ".. Suspended indefinitely"
      return state'
    Suspend (Just s) k -> do
      putStrLn $ ".. Suspended for " ++ show s ++ " seconds"
      evalPrint db task { computation = k nothing } state'
    Abort (Exception code m v) -> do
      putStrLn $ "** " ++ unpack m ++ formatValue v
      return state'
  where formatValue (Int 0) = ""
        formatValue v = " [" ++ unpack (toLiteral v) ++ "]"
