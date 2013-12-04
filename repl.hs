
module Main where

import System.Console.Readline
import System.Random
import System.Environment
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Control.Concurrent.STM
import Data.Text

import MOO.Parser
import MOO.Compiler
import MOO.Task
import MOO.Types
import MOO.Builtins
import MOO.Database
import MOO.Database.LambdaMOO

main :: IO ()
main = do
  case verifyBuiltins of
    Left  err -> putStrLn $ "Built-in function verification failed: " ++ err
    Right n   -> do
      putStrLn $ show n ++ " built-in functions verified"
      db <- newTVarIO =<< replDatabase
      gen <- getStdGen
      repLoop (initState db gen)

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

alterFrame st@State { stack = Stack (frame:stack) } f =
  st { stack = Stack (f frame : stack) }

run ":+d" state = return $ alterFrame state $
                  \frame -> frame { debugBit = True  }
run ":-d" state = return $ alterFrame state $
                  \frame -> frame { debugBit = False }

run ":stack" state = print (stack state) >> return state

run line state = case runParser expression initParserState "" (pack line) of
  Left err -> putStr "Parse error " >> print err >> return state
  Right expr -> do
    putStrLn $ "-- " ++ show expr
    env <- initEnvironment
    let comp = compileExpr expr `catchException` \(Exception code m v) -> do
          appendIO $ putStrLn $ "** " ++ unpack m ++ formatValue v
          return code
        formatValue (Int 0) = ""
        formatValue v = " [" ++ unpack (toLiteral v) ++ "]"
        contM  = runReaderT comp env
        stateM = runContT contM return
        stmM   = runStateT stateM state
    (value, state') <- atomically stmM
    queuedIO state'
    let state'' = state' { queuedIO = return () }
    putStrLn $ "=> " ++ unpack (toLiteral value)
    return state''
