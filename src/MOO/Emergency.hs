
{-# LANGUAGE OverloadedStrings #-}

module MOO.Emergency (emergencyMode) where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Concurrent.STM (TVar, readTVarIO, readTVar, writeTVar,
                               modifyTVar, atomically)
import Control.Monad (filterM, unless)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf, sort, nub, (\\))
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, catMaybes, isJust)
import Database.VCache (VTx, runVTx, readPVar, deref)
import System.Console.Haskeline (InputT, CompletionFunc, Completion(isFinished),
                                 runInputT, getInputLine,
                                 setComplete, defaultSettings,
                                 completeWordWithPrev, simpleCompletion)
import System.Exit (exitSuccess)

import qualified Data.HashMap.Strict as HM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Text as T

import MOO.Command
import MOO.Compiler
import MOO.Connection
import MOO.Database
import MOO.Object
import MOO.Parser
import MOO.Task
import MOO.Types
import MOO.Verb

import qualified MOO.List as Lst
import qualified MOO.String as Str

emergencyMode :: TVar World -> IO ()
emergencyMode world' = do
  world <- readTVarIO world'
  frame <- runVTx (persistenceVSpace $ persistence world) $
    mkFrame (database world)
  state <- setFrame frame <$> newState

  let player = initialPlayer frame
  console <- proxyConnection player
  atomically $ modifyTVar world' $ \world ->
    world { connections = M.insert player console (connections world) }

  pause
  putStrLn $ "\nEmergency Wizard Mode" ++ help

  let completion = mooCompletion world' player
  runInputT (setComplete completion defaultSettings) $ repLoop world' state

  atomically $ do
    world <- readTVar world'
    mapM_ closeConnection $ M.elems (connections world)
    writeTVar world' world { connections = M.empty }

  putStrLn "Continuing start-up...\n"

help :: String
help = " (type :help for assistance)"

mkFrame :: Database -> VTx StackFrame
mkFrame db = do
  wizards <- filterM isWizard $ allPlayers db
  let player = fromMaybe nothing $ listToMaybe wizards
  return $ becomePlayer player initFrame { verbFullName  = "Emergency Mode" }

  where isWizard oid = maybe False objectWizard <$> dbObject oid db

becomePlayer :: ObjId -> StackFrame -> StackFrame
becomePlayer player frame =
  let player' = Obj player
      insertVars = HM.insert "player" player' . HM.insert "me" player'
  in frame {
      variables     = insertVars (variables frame)
    , permissions   = player
    , initialPlayer = player
    }

currentPlayer :: TaskState -> ObjId
currentPlayer State { stack =
                        Stack [Frame { initialPlayer = player }] } = player

setFrame :: StackFrame -> TaskState -> TaskState
setFrame frame state = state { stack = Stack [frame] }

alterFrame :: TaskState -> (StackFrame -> StackFrame) -> TaskState
alterFrame state@State { stack = Stack (frame:frames) } f =
  state { stack = Stack (f frame : frames) }

mooCompletion :: TVar World -> ObjId -> CompletionFunc IO
mooCompletion world' player = completeWordWithPrev Nothing sep completions
  where sep = " \t.:$"

        completions ":" word = return $ map finishCompletion $
          filter (word `isPrefixOf`) $ sort
          ["debug", "player", "wizard", "continue", "shutdown", "help", "?"]
          where finishCompletion :: String -> Completion
                finishCompletion comp@"wizard" =
                  (simpleCompletion comp) { isFinished = False }
                finishCompletion comp = simpleCompletion comp

        completions prev word =
          fmap (mkCompletions $ null prev) $
          runTask =<< newTask world' player completionTask
          where completionTask =
                  getCompletions prev word `catchException` \_ -> return zero

        mkCompletions :: Bool -> Maybe Value -> [Completion]
        mkCompletions finished (Just (Lst v)) =
          mapMaybe (mkCompletion finished) (Lst.toList v)
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
              properties <- liftVTx $ mapM readPVar $
                HM.elems $ objectProperties obj
              return $ mkResults word $
                map fromId builtinProperties ++
                map (propertyName . deref) properties

        completeVerb :: String -> ObjId -> MOO Value
        completeVerb word oid = return zero

        completeCommandVerb :: String -> MOO Value
        completeCommandVerb word = do
          objects <- localObjects True
          verbs <- concat <$> mapM verbsForObject objects
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
          verbs <- map deref <$>
            liftVTx (mapM (readPVar . snd) $ objectVerbs obj)
          case objectParent obj of
            Nothing        -> return verbs
            Just parentOid -> do
              maybeParent <- getObject parentOid
              case maybeParent of
                Nothing     -> return verbs
                Just parent -> (verbs ++) <$> verbsForObject parent

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
                (fmap (maybe IS.empty objectContents) . getObject) maybeRoom
              let oids = maybe id (:) (if includeRoom
                                       then maybeRoom else Nothing) $
                         IS.toList (holding `IS.union` roomContents)
              (player' :) . catMaybes <$> mapM getObject (oids \\ [player])

        mkResults :: String -> [StrT] -> Value
        mkResults word = stringList . sort . nub . filter isPrefix
          where isPrefix name = word' `Str.isPrefixOf` name
                word' = Str.fromString word

repLoop :: TVar World -> TaskState -> InputT IO ()
repLoop world' state@State { stack = Stack [frame] } = do
  let player = initialPlayer frame
      perms  = permissions   frame
      who    | player == perms = "#" ++ show player
             | otherwise       = "#" ++ show player ++ "/#" ++ show perms
      prompt | debugBit frame  = "[" ++ who ++    "] "
             | otherwise       = "[" ++ who ++ " !d] "
  maybeLine <- getInputLine prompt
  case maybeLine of
    Nothing   -> return ()
    Just ""   -> repLoop world' state
    Just line -> liftIO (execute world' (trimr line) line state) >>=
      maybe (return ()) (repLoop world')

  where trimr :: String -> String
        trimr "" = ""
        trimr str
          | last str == ' ' = trimr (init str)
          | otherwise       = str

execute :: TVar World -> String -> String -> TaskState -> IO (Maybe TaskState)
execute _ help _ state
  | help == ":help" || help == ":?" = do
      mapM_ putStrLn [
          ";EXPR         Evaluate a MOO expression and print the result"
        , ";;CODE        Execute a whole MOO verb and print the result"
        , "COMMAND       Execute an arbitrary MOO command"
        , ":debug        Toggle the evaluation debug bit"
        , ":player #OBJ  Execute future commands as player #OBJ"
        , ":wizard!      Set the current player's wizard bit"
        , ":continue     Leave emergency mode and continue start-up"
        , ":shutdown     Exit the server (saving database changes)"
        , ":help, :?     Print this text"
        , ""
        , "Press TAB for completions"
        ]
      return (Just state)

execute _ ":debug" _ state = return $ Just $ alterFrame state $ \frame ->
  frame { debugBit = not (debugBit frame) }

execute world' (':':'p':'l':'a':'y':'e':'r':' ':obj) _ state
  | isJust obj' =
      let Just (Obj player) = obj'
          oldPlayer = currentPlayer state
      in do
        atomically $ modifyTVar world' $ \world ->
          let conns = connections world
              oldConn = conns M.! oldPlayer
          in world { connections = M.insert player oldConn $
                     M.delete oldPlayer conns }
        return (Just $ alterFrame state $ becomePlayer player)

  where obj' = parseObj (T.pack obj)

execute world' ":wizard!" _ state = do
  let player = currentPlayer state
  (vspace, db) <- (persistenceVSpace . persistence &&&
                   database) <$> readTVarIO world'
  runVTx vspace $ modifyObject player db $ \obj ->
    return obj { objectWizard = True }
  putStrLn $ ";#" ++ show player ++ ".wizard = 1"
  return (Just state)

execute _      ":continue" _ _ = return Nothing
execute world' ":shutdown" _ _ = do
  putStrLn "Synchronizing database and exiting..."
  syncPersistence . persistence =<< readTVarIO world'
  exitSuccess

execute _ (':':_) _ state = do
  putStrLn $ "Bad emergency mode command" ++ help
  return (Just state)

execute world' _ (';':';':line) state = evalP world' line state
execute world' _ (';'    :line) state = evalE world' line state
execute world' _          line  state = evalC world' line state

-- wait for output to flush
pause :: IO ()
pause = delay 50000

-- Check that we have not been disconnected
checkConnected :: TVar World -> TaskState -> IO (Maybe TaskState)
checkConnected world' state = do
  conns <- connections <$> readTVarIO world'
  return $ if currentPlayer state `M.member` conns then Just state else Nothing

evalC :: TVar World -> String -> TaskState -> IO (Maybe TaskState)
evalC world' line state@State { stack = Stack [frame] } = do
  let player  = initialPlayer frame
      command = parseCommand (T.pack line)
  runTask =<< newTask world' player (runCommand command)
  pause
  checkConnected world' state

evalE :: TVar World -> String -> TaskState -> IO (Maybe TaskState)
evalE world' line state@State { stack = Stack [frame] } =
  case parseExpr (T.pack line) of
    Left err -> putStr "Parse error " >> print err >> return (Just state)
    Right expr -> eval state =<<
                  newTask world' (initialPlayer frame) (evaluate expr)

evalP :: TVar World -> String -> TaskState -> IO (Maybe TaskState)
evalP world' line state@State { stack = Stack [frame] } =
  case parseProgram (T.pack $ line ++ ";") of
    Left errs -> mapM_ putStrLn errs >> return (Just state)
    Right program -> eval state =<<
      newTask world' (initialPlayer frame) (compile program)

eval :: TaskState -> Task -> IO (Maybe TaskState)
eval state task = do
  state' <- taskState <$>
            evalPrint task { taskState = state {
                                  ticksLeft = ticksLeft (taskState task)
                                , startTime = startTime (taskState task)
                                } }

  atomically $ modifyTVar (taskWorld task) $ \world ->
    world { tasks = M.delete (taskId task) $ tasks world }

  checkConnected (taskWorld task) state'

evalPrint :: Task -> IO Task
evalPrint task = do
  (result, task') <- stepTask task
  pause
  case result of
    Complete value -> do
      putStrLn $ "=> " ++ T.unpack (toLiteral value)
      return task'
    Suspend _ -> do
      putStrLn "=> (suspended)"
      return task'
    RequestIO io (Resume k) -> do
      result <- io
      evalPrint task' { taskComputation = k result }
    Uncaught exception -> do
      notifyLines $ map Str.fromText $ formatTraceback exception
      return task'
    Timeout resource callStack -> do
      let exception = timeoutException resource callStack
      notifyLines $ map Str.fromText $ formatTraceback exception
      return task'
    Suicide -> do
      putStrLn "=> (task killed itself)"
      return task'

  where notifyLines :: [StrT] -> IO ()
        notifyLines = mapM_ (putStrLn . Str.toString)
