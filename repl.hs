
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
main =
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

repLoop :: TVar Database -> TaskState -> IO ()
repLoop db state = do
  maybeLine <- readline ">> "
  case maybeLine of
    Nothing   -> return ()
    Just line -> do
      addHistory line
      run db line state >>= repLoop db

addFrame :: StackFrame -> TaskState -> TaskState
addFrame frame st@State { stack = Stack frames } =
  st { stack = Stack (frame : frames) }

mkTestFrame :: Database -> STM StackFrame
mkTestFrame db = do
  wizards <- filterM isWizard $ allPlayers db
  return initFrame { permissions = fromMaybe (-1) $ listToMaybe wizards }
  where isWizard oid = fmap (maybe False objectWizard) $ dbObject oid db

alterFrame :: TaskState -> (StackFrame -> StackFrame) -> TaskState
alterFrame st@State { stack = Stack (frame:stack) } f =
  st { stack = Stack (f frame : stack) }

run :: TVar Database -> String -> TaskState -> IO TaskState
run _ ":+d" state = return $ alterFrame state $
                  \frame -> frame { debugBit = True  }
run _ ":-d" state = return $ alterFrame state $
                  \frame -> frame { debugBit = False }

run _ (':':'p':'e':'r':'m':' ':perm) state =
  return $ alterFrame state $ \frame -> frame { permissions = read perm }

run _ ":stack" state = print (stack state) >> return state

run db (';':';':line) state = evalP db line state
run db (';'    :line) state = evalE db line state
run db          line  state = evalE db line state

evalE :: TVar Database -> String -> TaskState -> IO TaskState
evalE db line state =
  case runParser (between whiteSpace eof expression)
       initParserState "" (pack line) of
    Left err -> putStr "Parse error " >> print err >> return state
    Right expr -> do
      -- putStrLn $ "-- " ++ show expr
      task <- initTask db (evaluate expr)
      fmap taskState $ evalPrint task { taskState = state }

evalP :: TVar Database -> String -> TaskState -> IO TaskState
evalP db line state =
  case runParser program initParserState "" (pack line) of
    Left err -> putStr "Parse error" >> print err >> return state
    Right program -> do
      -- putStrLn $ "-- " ++ show program
      task <- initTask db (compile program)
      fmap taskState $ evalPrint task { taskState = state }

evalPrint :: Task -> IO Task
evalPrint task = do
  (result, task') <- runTask task
  case result of
    Complete value -> do
      putStrLn $ "=> " ++ unpack (toLiteral value)
      return task'
    Suspend Nothing _  -> do
      putStrLn   ".. Suspended indefinitely"
      return task'
    Suspend (Just s) (Resume k) -> do
      putStrLn $ ".. Suspended for " ++ show s ++ " seconds"
      evalPrint task' { taskComputation = k nothing }
    Abort (Exception _ m v) _ -> do
      putStrLn $ "** " ++ unpack m ++ formatValue v
      return task'
  where formatValue (Int 0) = ""
        formatValue v = " [" ++ unpack (toLiteral v) ++ "]"
