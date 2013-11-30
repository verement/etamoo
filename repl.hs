
module Main where

import System.Console.Readline
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Data.Text

import MOO.Parser
import MOO.Compiler
import MOO.Execution
import MOO.Types
import MOO.Builtins

main :: IO ()
main = do
  case verifyBuiltins of
    Left  err -> putStrLn $ "Built-in function verification failed: " ++ err
    Right n   -> do
      putStrLn $ show n ++ " built-in functions verified"
      repLoop initStack

repLoop stack = do
  maybeLine <- readline ">> "
  case maybeLine of
    Nothing   -> return ()
    Just line -> do
      addHistory line
      run line stack >>= repLoop

alterFrame (Stack (x:xs)) f = Stack (f x : xs)

run ":+d" stack = return $ alterFrame stack $
                  \frame -> frame { debugBit = True  }
run ":-d" stack = return $ alterFrame stack $
                  \frame -> frame { debugBit = False }

run ":stack" stack = print stack >> return stack

run line stack = case runParser expression initParserState "" (pack line) of
  Left err -> putStr "Parse error " >> print err >> return stack
  Right expr -> do
    putStrLn $ "-- " ++ show expr
    let comp = compileExpr expr `catchException` \(Exception code m v) -> do
          liftIO $ putStrLn $ "** " ++ unpack m ++ formatValue v
          return code
        formatValue (Int 0) = ""
        formatValue v = " [" ++ unpack (toLiteral v) ++ "]"
        cont  = runReaderT comp initEnvironment
        state = runContT cont return
        io    = runStateT state stack
    (value, stack') <- io
    putStrLn $ "=> " ++ unpack (toLiteral value)
    return stack'
