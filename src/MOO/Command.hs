
{-# LANGUAGE OverloadedStrings #-}

-- | MOO command parsing and execution
module MOO.Command (

  -- * Data Structures
    Command(..)

  -- * Parsing Typed Commands
  , parseWords
  , parseCommand

  -- * Executing MOO Code
  , runCommand

  ) where

import Control.Applicative ((<$>))
import Control.Monad (void, foldM, join)
import Data.Char (isSpace, isDigit)
import Data.Monoid (Monoid(mempty, mappend, mconcat), First(First, getFirst))
import Data.Text (Text)
import Text.Parsec (parse, try, many, many1, char, anyChar, noneOf, spaces,
                    satisfy, between, eof, (<|>))
import Text.Parsec.Text (Parser)

import qualified Data.IntSet as IS
import qualified Data.Text as T
import qualified Data.Vector as V

import {-# SOURCE #-} MOO.Connection
import MOO.Object
import {-# SOURCE #-} MOO.Task
import MOO.Types
import MOO.Verb

import qualified MOO.String as Str

commandWord :: Parser Text
commandWord = do
  word <- many1 wordChar
  spaces
  return (T.concat word)

  where wordChar = T.singleton <$> satisfy nonspecial <|>
                   T.pack      <$> quotedChars        <|>
                   T.singleton <$> backslashChar      <|>
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
  argstr <- T.pack <$> many anyChar
  return (verb, argstr)

matchPrep :: [StrT] -> (StrT, (PrepSpec, StrT), StrT)
matchPrep = matchPrep' id prepPhrases
  where matchPrep' dobj _ [] = (Str.unwords $ dobj [], (PrepNone, ""), "")
        matchPrep' dobj ((spec,phrase):phrases) args
          | phrase == argsPhrase =
            (Str.unwords $ dobj [], (spec, Str.unwords argsPhrase),
             Str.unwords iobj)
          | otherwise = matchPrep' dobj phrases args
          where (argsPhrase, iobj) = splitAt (length phrase) args
        matchPrep' dobj [] (arg:args) =
          matchPrep' (dobj . (arg :)) prepPhrases args

-- | A structure describing a player's parsed command
data Command = Command {
    commandVerb     :: StrT
  , commandArgs     :: [StrT]
  , commandArgStr   :: StrT
  , commandDObjStr  :: StrT
  , commandPrepSpec :: PrepSpec
  , commandPrepStr  :: StrT
  , commandIObjStr  :: StrT
  } deriving Show

-- | Split a typed command into words according to the MOO rules for quoting
-- and escaping.
parseWords :: Text -> [StrT]
parseWords argstr = map Str.fromText args
  where Right args = parse commandWords "" argstr

-- | Return a 'Command' value describing the arguments of a typed command as
-- parsed into verb, preposition, direct and indirect object, etc.
parseCommand :: Text -> Command
parseCommand cmd = Command {
    commandVerb     = Str.fromText verb
  , commandArgs     = args
  , commandArgStr   = Str.fromText argstr
  , commandDObjStr  = dobjstr
  , commandPrepSpec = prepSpec
  , commandPrepStr  = prepstr
  , commandIObjStr  = iobjstr
  }
  where Right (verb, argstr) = parse command "" cmd
        args = parseWords argstr
        (dobjstr, (prepSpec, prepstr), iobjstr) = matchPrep args

objectNumber :: Parser ObjId
objectNumber = fmap read $ between (char '#') eof $ many1 (satisfy isDigit)

matchObject :: ObjId -> StrT -> MOO ObjId
matchObject player str
  | Str.null str = return nothing
  | otherwise    = case parse objectNumber "" (Str.toText str) of
    Right oid -> do
      obj <- getObject oid
      case obj of
        Just _  -> return oid
        Nothing -> matchObject' player str
    Left _ -> matchObject' player str

  where matchObject' :: ObjId -> StrT -> MOO ObjId
        matchObject' player str = case str of
          "me"   -> return player
          "here" -> maybe nothing (objectForMaybe . objectLocation) <$>
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
                  (fmap (maybe IS.empty objectContents) . getObject)
                  maybeRoom
                matchName str $ IS.toList (holding `IS.union` roomContents)

        matchName :: StrT -> [ObjId] -> MOO ObjId
        matchName str = fmap (uncurry matchResult) .
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
          | str ==               name = ExactMatch
          | str `Str.isPrefixOf` name = PrefixMatch
          | otherwise                 = NoMatch
        nameMatch _ _ = NoMatch

data Match = NoMatch | PrefixMatch | ExactMatch

instance Monoid Match where
  mempty = NoMatch

  NoMatch `mappend` match      = match
  _       `mappend` ExactMatch = ExactMatch
  match   `mappend` _          = match

-- | Execute a typed command by locating and calling an appropriate MOO verb
-- for the current player, matching @dobj@ and @iobj@ objects against the
-- strings in the typed command.
runCommand :: Command -> MOO Value
runCommand Command { commandVerb = "" } = return zero
runCommand command = do
  player <- getPlayer
  dobj <- matchObject player (commandDObjStr command)
  iobj <- matchObject player (commandIObjStr command)

  room <- objectForMaybe . join . fmap objectLocation <$> getObject player
  maybeVerb <- getFirst . mconcat . map First <$>
               mapM (locateVerb dobj iobj) [player, room, dobj, iobj]
  case maybeVerb of
    Just (this, spec) -> callCommandVerb player spec this command (dobj, iobj)
    Nothing           -> do
      maybeVerb <- findVerb verbPermX "huh" room
      case maybeVerb of
        (Just oid, Just verb) ->
          callCommandVerb player (oid, verb) room command (dobj, iobj)
        _ -> notify player "I couldn't understand that." >> return zero

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
