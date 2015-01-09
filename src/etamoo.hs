
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO,
                               readTVarIO, readTVar, modifyTVar)
import Control.Monad (liftM, filterM, unless)
import Control.Monad.IO.Class (liftIO)
import Data.List (sort, nub, (\\))
import Data.Maybe (mapMaybe, catMaybes, listToMaybe, fromMaybe)
import Network (withSocketsDo)
import System.Console.Haskeline (InputT, CompletionFunc, Completion(..),
                                 runInputT, getInputLine,
                                 setComplete, defaultSettings,
                                 completeWordWithPrev, simpleCompletion)
import System.Environment (getArgs)
import System.Posix (installHandler, sigPIPE, Handler(..))

import qualified Data.HashMap.Strict as HM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

import MOO.Builtins
import MOO.Command
import MOO.Compiler
import MOO.Database
import MOO.Database.LambdaMOO
import MOO.Object
import MOO.Task
import MOO.Types
import MOO.Parser
import MOO.Verb

import qualified MOO.String as Str

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

      let completion = mooCompletion worldTVar (initialPlayer testFrame)
      runInputT (setComplete completion defaultSettings) $
        repLoop worldTVar $ addFrame testFrame state

replDatabase :: IO Database
replDatabase = do
  args <- getArgs
  case args of
    [dbFile] -> loadLMDatabase dbFile >>= either (error . show) return
    []       -> return initDatabase

mooCompletion :: TVar World -> ObjId -> CompletionFunc IO
mooCompletion world player = completeWordWithPrev Nothing sep completions
  where sep = " \t.:$"

        completions prev word =
          liftM (mkCompletions $ null prev) $
          runTask =<< newTask world player completionTask
          where completionTask =
                  getCompletions prev word `catchException` \_ -> return zero

        mkCompletions :: Bool -> Maybe Value -> [Completion]
        mkCompletions finished (Just (Lst v)) =
          mapMaybe (mkCompletion finished) (V.toList v)
        mkCompletions _ _ = []

        mkCompletion :: Bool -> Value -> Maybe Completion
        mkCompletion finished (Str str) =
          Just $ (simpleCompletion $ Str.toString str) { isFinished = finished }
        mkCompletion _ _ = Nothing

        getCompletions :: String -> String -> MOO Value
        getCompletions "" word = completeCommandVerb word
        getCompletions ('$':_) word = completeProperty word 0
        getCompletions ('.':prev) word =
          objectForCompletion prev >>= completeProperty word
        getCompletions (':':prev) word =
          objectForCompletion prev >>= completeVerb word
        getCompletions _ word = completeName word

        objectForCompletion :: String -> MOO ObjId
        objectForCompletion prev = return nothing

        completeProperty :: String -> ObjId -> MOO Value
        completeProperty word oid = do
          maybeObj <- getObject oid
          case maybeObj of
            Nothing  -> return zero
            Just obj -> do
              unless (objectPermR obj) $ checkPermission (objectOwner obj)
              properties <- liftSTM $ mapM readTVar $
                            HM.elems $ objectProperties obj
              return $ mkResults word $
                map fromId builtinProperties ++ map propertyName properties

        completeVerb :: String -> ObjId -> MOO Value
        completeVerb word oid = return zero

        completeCommandVerb :: String -> MOO Value
        completeCommandVerb word = do
          objects <- localObjects True
          verbs <- concat `liftM` mapM verbsForObject objects
          return $ mkResults word $ concatMap simpleVerbNames $
            filter isCommandVerb verbs

        simpleVerbNames :: Verb -> [StrT]
        simpleVerbNames = map removeStar . Str.words . verbNames
          where removeStar name =
                  let (before, after) = Str.break (== '*') name
                  in before `Str.append` if Str.null after
                                         then after else Str.tail after

        isCommandVerb :: Verb -> Bool
        isCommandVerb verb =
          not $ verbDirectObject   verb /= ObjNone  &&
                verbPreposition    verb == PrepNone &&
                verbIndirectObject verb /= ObjNone

        verbsForObject :: Object -> MOO [Verb]
        verbsForObject obj = do
          verbs <- liftSTM $ mapM (readTVar . snd) $ objectVerbs obj
          case objectParent obj of
            Nothing        -> return verbs
            Just parentOid -> do
              maybeParent <- getObject parentOid
              case maybeParent of
                Nothing     -> return verbs
                Just parent -> (verbs ++) `liftM` verbsForObject parent

        completeName :: String -> MOO Value
        completeName word = do
          objects <- localObjects False
          return $ mkResults word $ map objectName objects ++ ["me", "here"]

        localObjects :: Bool -> MOO [Object]
        localObjects includeRoom = do
          player <- getPlayer
          maybePlayer <- getObject player
          case maybePlayer of
            Nothing      -> return []
            Just player' -> do
              let holding   = objectContents player'
                  maybeRoom = objectLocation player'
              roomContents <-
                maybe (return IS.empty)
                (liftM (maybe IS.empty objectContents) . getObject) maybeRoom
              let oids = maybe id (:) (if includeRoom
                                       then maybeRoom else Nothing) $
                         IS.toList (holding `IS.union` roomContents)
              liftM ((player' :) . catMaybes) $
                mapM getObject (oids \\ [player])

        mkResults :: String -> [StrT] -> Value
        mkResults word = stringList . sort . nub . filter isPrefix
          where isPrefix name = word' `Str.isPrefixOf` name
                word' = Str.fromString word

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
  let player = fromMaybe nothing $ listToMaybe wizards
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
      command = parseCommand (T.pack line)
  runTask =<< newTask world player (runCommand command)
  return state

evalE :: TVar World -> String -> TaskState -> IO TaskState
evalE world line state@State { stack = Stack (frame:_) } =
  case runParser (between whiteSpace eof expression)
       initParserState "" (T.pack line) of
    Left err -> putStr "Parse error " >> print err >> return state
    Right expr -> eval state =<<
                  newTask world (initialPlayer frame) (evaluate expr)

evalP :: TVar World -> String -> TaskState -> IO TaskState
evalP world line state@State { stack = Stack (frame:_) } =
  case runParser program initParserState "" (T.pack line) of
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
      putStrLn $ "=> " ++ T.unpack (toLiteral value)
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
      putStrLn $ "** " ++ Str.toString message ++ formatValue value
      return task'
    Timeout resource callStack -> do
      let exception = timeoutException resource callStack
      notifyLines $ formatTraceback exception
      putStrLn $ "!! " ++ Str.toString (exceptionMessage exception)
      return task'
    Suicide -> do
      putStrLn   "-- Task killed itself"
      return task'

  where formatValue (Int 0) = ""
        formatValue v = " [" ++ T.unpack (toLiteral v) ++ "]"

        notifyLines :: [StrT] -> IO ()
        notifyLines = mapM_ (putStrLn . Str.toString)
