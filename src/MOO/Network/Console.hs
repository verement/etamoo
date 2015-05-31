
module MOO.Network.Console ( createConsoleListener ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (STM, TVar, atomically, readTVar)
import Control.Exception (SomeException, try)
import Control.Monad (liftM, when, unless, forever)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.List ((\\), sort, nub)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Pipes (Producer, Pipe, runEffect, await, yield, for, cat, (>->))
import Pipes.ByteString (stdout)
import Pipes.Concurrent (spawn, unbounded, send, fromInput, toOutput)
import System.Console.Haskeline (InputT, CompletionFunc, Completion(),
                                 runInputT, getInputLine,
                                 setComplete, defaultSettings,
                                 completeWordWithPrev, simpleCompletion)
import System.IO (hIsClosed, stdin)

import qualified Data.HashMap.Strict as HM
import qualified Data.IntSet as IS
import qualified Data.Text as T
import qualified Data.Vector as V

import MOO.Connection (ConnectionHandler)
import {-# SOURCE #-} MOO.Network (Point(Console),
                                   Listener(listenerPoint, listenerCancel))
import MOO.Object
import MOO.Task
import MOO.Types
import MOO.Verb

import qualified MOO.String as Str

createConsoleListener :: Listener -> ConnectionHandler -> IO Listener
createConsoleListener listener handler = do
  let Console world' = listenerPoint listener

  thread <- forkIO $ acceptConnection world' handler

  return listener { listenerCancel = killThread thread }

acceptConnection :: TVar World -> ConnectionHandler -> IO ()
acceptConnection worldTVar handler = do
  let connectionName :: STM String
      connectionName = return "console"

  (output, input) <- spawn unbounded

  thread <- forkIO $ runInputT defaultSettings $ runEffect $
    consoleInput >-> writeLines >-> writeUtf8 >-> toOutput output

  handler connectionName (fromInput input, stdout)
  killThread thread

  eof <- try (hIsClosed stdin) >>=
         either (\err -> let _ = err :: SomeException in return True) return
  unless eof $ acceptConnection worldTVar handler

  where writeLines :: Monad m => Pipe Text Text m ()
        writeLines = forever $ await >>= yield . flip T.snoc '\n'

        writeUtf8 :: Monad m => Pipe Text ByteString m ()
        writeUtf8 = for cat (yield . encodeUtf8)

consoleInput :: Producer Text (InputT IO) ()
consoleInput = loop
  where loop = do
          maybeLine <- lift $ getInputLine ""
          case maybeLine of
            Just line -> yield (T.pack line) >> loop
            Nothing   -> return ()

{-
  let completion = mooCompletion worldTVar (initialPlayer testFrame)
  runInputT (setComplete completion defaultSettings) $
    repLoop worldTVar $ addFrame testFrame state

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
-}
