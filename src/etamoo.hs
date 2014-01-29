
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text
import Network (withSocketsDo)
import System.Console.Haskeline
import System.Environment
import System.Posix (installHandler, sigPIPE, Handler(..))

import MOO.Parser
import MOO.Compiler
import MOO.Task
import MOO.Types
import MOO.Builtins
import MOO.Database
import MOO.Database.LambdaMOO
import MOO.Object
import MOO.Command

import qualified Data.Map as M

main :: IO ()
main = withSocketsDo $ do
  installHandler sigPIPE Ignore Nothing

  case verifyBuiltins of
    Left  err -> putStrLn $ "Built-in function verification failed: " ++ err
    Right n   -> do
      putStrLn $ show n ++ " built-in functions verified"
      db <- replDatabase
      worldTVar <- newTVarIO initWorld { database = db }
      testFrame <- atomically $ mkTestFrame db
      state <- newState

      runInputT (setComplete noCompletion defaultSettings) $
        repLoop worldTVar $ addFrame testFrame state

replDatabase :: IO Database
replDatabase = do
  args <- getArgs
  case args of
    [dbFile] -> loadLMDatabase dbFile >>= either (error . show) return
    []       -> return initDatabase

repLoop :: TVar World -> TaskState -> InputT IO ()
repLoop world state = do
  maybeLine <- getInputLine ">> "
  case maybeLine of
    Nothing   -> return ()
    Just ""   -> repLoop world state
    Just line -> repLoop world =<< liftIO (run world line state)

addFrame :: StackFrame -> TaskState -> TaskState
addFrame frame state@State { stack = Stack frames } =
  state { stack = Stack (frame : frames) }

mkTestFrame :: Database -> STM StackFrame
mkTestFrame db = do
  wizards <- filterM isWizard $ allPlayers db
  let player = fromMaybe (-1) $ listToMaybe wizards
  return initFrame {
      variables     = mkVariables [("player", Obj player)]
    , permissions   = player
    , verbFullName  = "REPL"
    , initialPlayer = player
    }
  where isWizard oid = maybe False objectWizard `liftM` dbObject oid db

alterFrame :: TaskState -> (StackFrame -> StackFrame) -> TaskState
alterFrame state@State { stack = Stack (frame:stack) } f =
  state { stack = Stack (f frame : stack) }

run :: TVar World -> String -> TaskState -> IO TaskState
run _ ":+d" state = return $ alterFrame state $
                  \frame -> frame { debugBit = True  }
run _ ":-d" state = return $ alterFrame state $
                  \frame -> frame { debugBit = False }

run _ (':':'p':'e':'r':'m':' ':perm) state =
  return $ alterFrame state $ \frame -> frame { permissions = read perm }

run _ ":stack" state = print (stack state) >> return state

run world' ":tasks" state = do
  world <- readTVarIO world'
  print (M.elems $ tasks world)
  return state

run world (';':';':line) state = evalP world line state
run world (';'    :line) state = evalE world line state
run world          line  state = evalC world line state

evalC :: TVar World -> String -> TaskState -> IO TaskState
evalC world line state@State { stack = Stack (frame:_) } = do
  let player  = initialPlayer frame
      command = parseCommand (pack line)
  runTask =<< newTask world player (runCommand command)
  return state

evalE :: TVar World -> String -> TaskState -> IO TaskState
evalE world line state@State { stack = Stack (frame:_) } =
  case runParser (between whiteSpace eof expression)
       initParserState "" (pack line) of
    Left err -> putStr "Parse error " >> print err >> return state
    Right expr -> eval state =<<
                  newTask world (initialPlayer frame) (evaluate expr)

evalP :: TVar World -> String -> TaskState -> IO TaskState
evalP world line state@State { stack = Stack (frame:_) } =
  case runParser program initParserState "" (pack line) of
    Left err -> putStr "Parse error" >> print err >> return state
    Right program -> eval state =<<
                     newTask world (initialPlayer frame) (compile program)

eval :: TaskState -> Task -> IO TaskState
eval state task = do
  state' <- taskState `liftM`
            evalPrint task { taskState = state {
                                  ticksLeft = ticksLeft (taskState task)
                                , startTime = startTime (taskState task)
                                } }

  atomically $ modifyTVar (taskWorld task) $ \world ->
    world { tasks = M.delete (taskId task) $ tasks world }

  return state'

evalPrint :: Task -> IO Task
evalPrint task = do
  (result, task') <- stepTask task
  case result of
    Complete value -> do
      putStrLn $ "=> " ++ unpack (toLiteral value)
      return task'
    Suspend Nothing _  -> do
      putStrLn   ".. Suspended indefinitely"
      return task'
    Suspend (Just usecs) (Resume resume) -> do
      let secs = (fromIntegral usecs :: Double) / 1000000
      putStrLn $ ".. Suspended for " ++ show secs ++ " seconds"
      evalPrint task' { taskComputation = resume () }
    RequestIO io (Resume k) -> do
      result <- io
      evalPrint task' { taskComputation = k result }
    Uncaught exception@Exception {
        exceptionMessage = message
      , exceptionValue   = value
      } -> do
      notifyLines $ formatTraceback exception
      putStrLn $ "** " ++ unpack message ++ formatValue value
      return task'
    Timeout resource callStack -> do
      let exception = timeoutException resource callStack
      notifyLines $ formatTraceback exception
      putStrLn $ "!! " ++ unpack (exceptionMessage exception)
      return task'
    Suicide -> do
      putStrLn   "-- Task killed itself"
      return task'

  where formatValue (Int 0) = ""
        formatValue v = " [" ++ unpack (toLiteral v) ++ "]"

        notifyLines :: [Text] -> IO ()
        notifyLines = mapM_ (putStrLn . unpack)
