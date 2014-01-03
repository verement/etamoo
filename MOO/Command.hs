
{-# LANGUAGE OverloadedStrings #-}

module MOO.Command ( Command(..)
                   , parseWords
                   , parseCommand
                   , runCommand
                   ) where

import Control.Monad (liftM, void, foldM, join)
import Data.Char (isSpace, isDigit)
import Data.Monoid
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text (Parser)

import qualified Data.IntSet as IS
import qualified Data.Text as T
import qualified Data.Vector as V

import MOO.Types
import {-# SOURCE #-} MOO.Task
import MOO.Object
import MOO.Verb
import MOO.Network

commandWord :: Parser Text
commandWord = do
  word <- many1 wordChar
  spaces
  return (T.concat word)

  where wordChar = T.singleton `liftM` satisfy nonspecial <|>
                   T.pack      `liftM` quotedChars        <|>
                   T.singleton `liftM` backslashChar      <|>
                   trailingBackslash

        nonspecial '\"' = False
        nonspecial '\\' = False
        nonspecial  c   = not $ isSpace c

        quotedChars     = between (char '"') quoteEnd $
                          many (noneOf "\"\\" <|> backslashChar)
        quoteEnd        = void (char '"') <|> eof <|> void trailingBackslash

        backslashChar   = try (char '\\' >> anyChar)

        trailingBackslash = try (char '\\' >> eof) >> return ""

commandWords :: Parser [Text]
commandWords = between spaces eof $ many commandWord

builtinCommand :: Parser Text
builtinCommand = say <|> emote <|> eval
  where say   = char '\"' >> return "say"
        emote = char ':'  >> return "emote"
        eval  = char ';'  >> return "eval"

command :: Parser (Text, Text)
command = between spaces eof $ do
  verb <- builtinCommand <|> commandWord <|> return ""
  argstr <- T.pack `liftM` many anyChar
  return (verb, argstr)

matchPrep :: [Text] -> (Text, (PrepSpec, Text), Text)
matchPrep = matchPrep' id prepPhrases
  where matchPrep' dobj _ [] = (T.unwords $ dobj [], (PrepNone, ""), "")
        matchPrep' dobj ((spec,phrase):phrases) args
          | phrase == map T.toCaseFold argsPhrase =
            (T.unwords $ dobj [], (spec, T.unwords argsPhrase), T.unwords iobj)
          | otherwise = matchPrep' dobj phrases args
          where (argsPhrase, iobj) = splitAt (length phrase) args
        matchPrep' dobj [] (arg:args) =
          matchPrep' (dobj . (arg :)) prepPhrases args

data Command = Command {
    commandVerb     :: Text
  , commandArgs     :: [Text]
  , commandArgStr   :: Text
  , commandDObjStr  :: Text
  , commandPrepSpec :: PrepSpec
  , commandPrepStr  :: Text
  , commandIObjStr  :: Text
  } deriving Show

parseWords :: Text -> [Text]
parseWords argstr = args
  where Right args = parse commandWords "" argstr

parseCommand :: Text -> Command
parseCommand cmd = Command verb args argstr dobjstr prepSpec prepstr iobjstr
  where Right (verb, argstr) = parse command "" cmd
        args = parseWords argstr
        (dobjstr, (prepSpec, prepstr), iobjstr) = matchPrep args

objectNumber :: Parser ObjId
objectNumber = liftM read $ between (char '#') eof $ many1 (satisfy isDigit)

matchObject :: ObjId -> StrT -> MOO ObjId
matchObject player str
  | T.null str = return nothing
  | otherwise  = case parse objectNumber "" str of
    Right oid -> do
      obj <- getObject oid
      case obj of
        Just _  -> return oid
        Nothing -> matchObject' player str
    Left _ -> matchObject' player str

  where matchObject' :: ObjId -> StrT -> MOO ObjId
        matchObject' player str = case str' of
          "me"   -> return player
          "here" -> maybe nothing (objectForMaybe . objectLocation) `liftM`
                    getObject player
          _      -> do
            maybePlayer <- getObject player
            case maybePlayer of
              Nothing      -> return failedMatch
              Just player' -> do
                let holding   = objectContents player'
                    maybeRoom = objectLocation player'
                roomContents <-
                  maybe (return IS.empty)
                  (liftM (maybe IS.empty objectContents) . getObject)
                  maybeRoom
                matchName str' $ IS.toList (holding `IS.union` roomContents)
          where str' = T.toCaseFold str

        matchName :: StrT -> [ObjId] -> MOO ObjId
        matchName str = liftM (uncurry matchResult) .
                        foldM (matchName' str) ([], [])
        matchName' str matches@(exact, prefix) oid = do
          maybeAliases <- readProperty oid "aliases"
          maybeObj <- getObject oid
          let aliases = case maybeAliases of
                Just (Lst v) -> V.toList v
                _            -> []
              names = maybe id ((:) . Str . objectName) maybeObj aliases
          return $ case searchNames str names of
            ExactMatch  -> (oid : exact, prefix)
            PrefixMatch -> (exact, oid : prefix)
            NoMatch     -> matches

        matchResult :: [ObjId] -> [ObjId] -> ObjId
        matchResult [oid] _     = oid
        matchResult []    [oid] = oid
        matchResult []    []    = failedMatch
        matchResult _     _     = ambiguousMatch

        searchNames :: StrT -> [Value] -> Match
        searchNames str = mconcat . map (nameMatch str)

        nameMatch :: StrT -> Value -> Match
        nameMatch str (Str name)
          | name'               == str = ExactMatch
          | T.take strLen name' == str = PrefixMatch
          | otherwise                  = NoMatch
          where name'  = T.toCaseFold name
                strLen = T.length str
        nameMatch _ _ = NoMatch

        nothing        = -1
        ambiguousMatch = -2
        failedMatch    = -3

data Match = NoMatch | PrefixMatch | ExactMatch

instance Monoid Match where
  mempty = NoMatch

  NoMatch `mappend` match      = match
  _       `mappend` ExactMatch = ExactMatch
  match   `mappend` _          = match

runCommand :: ObjId -> Command -> MOO Value
runCommand player command = do
  dobj <- matchObject player (commandDObjStr command)
  iobj <- matchObject player (commandIObjStr command)

  room <- (objectForMaybe . join . fmap objectLocation) `liftM` getObject player
  maybeVerb <- (getFirst . mconcat . map First) `liftM`
               mapM (locateVerb dobj iobj) [player, room, dobj, iobj]
  case maybeVerb of
    Just (this, spec) -> callCommandVerb player spec this command (dobj, iobj)
    Nothing           -> do
      maybeVerb <- findVerb verbPermX "huh" room
      case maybeVerb of
        (Just oid, Just verb) ->
          callCommandVerb player (oid, verb) room command (dobj, iobj)
        _ -> notify player "I couldn't understand that." >> return nothing

  where locateVerb :: ObjId -> ObjId -> ObjId ->
                      MOO (Maybe (ObjId, (ObjId, Verb)))
        locateVerb dobj iobj this = do
          location <- findVerb acceptable (commandVerb command) this
          case location of
            (Just oid, Just verb) -> return $ Just (this, (oid, verb))
            _                     -> return Nothing
          where acceptable verb =
                  objMatch this (verbDirectObject   verb) dobj &&
                  objMatch this (verbIndirectObject verb) iobj &&
                  prepMatch (verbPreposition verb) (commandPrepSpec command)
