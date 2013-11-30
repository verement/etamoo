
module Main where

import System.Console.Readline
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Data.Text

import MOO.Parser
import MOO.Compiler
import MOO.Execution
import MOO.Types

main :: IO ()
main = repLoop initStack

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
    let comp = compileExpr expr `catchException` \(Exception code m _) -> do
          liftIO $ putStrLn $ "** " ++ unpack m
          return code
        cont  = runReaderT comp initEnvironment
        state = runContT cont return
        io    = runStateT state stack
    (value, stack') <- io
    putStrLn $ "=> " ++ unpack (toLiteral value)
    return stack'
