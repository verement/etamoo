
module Main where

import MOO.Parser
import MOO.Compiler
import MOO.Types
import System.Console.Readline
import Data.Text
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Reader

main :: IO ()
main = do
  maybeLine <- readline ">> "
  case maybeLine of
    Nothing   -> return ()
    Just line -> do
      addHistory line
      run line
      main

run line = case runParser expression initParserState "" (pack line) of
  Left err -> print err
  Right expr -> do
    print expr
    let comp = compileExpr expr `catchException` \(Exception code m _) -> do
          liftIO $ putStrLn $ "** " ++ unpack m
          return code
    val <- runContT (evalStateT (runReaderT comp initEnvironment) initStack)
           return
    putStrLn $ "=> " ++ show val
