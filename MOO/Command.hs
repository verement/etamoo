
{-# LANGUAGE OverloadedStrings #-}

module MOO.Command ( Command(..)
                   , parseCommand
                   ) where

import Control.Monad (liftM, void)
import Data.Char (isSpace)
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text (Parser)

import qualified Data.Text as T

import MOO.Verb

replaceCharCommand :: Text -> Text
replaceCharCommand cmd
  | T.null begin = cmd
  | otherwise    = case T.head begin of
    '"' -> T.concat [whitespace, "say "  , rest]
    ':' -> T.concat [whitespace, "emote ", rest]
    ';' -> T.concat [whitespace, "eval " , rest]
    _   -> cmd
  where (whitespace, begin) = T.span isSpace cmd
        rest = T.tail begin

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

command :: Parser (Text, Text)
command = between spaces eof $ do
  verb <- commandWord <|> return ""
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

parseCommand :: Text -> Command
parseCommand cmd = Command verb args argstr dobjstr prepSpec prepstr iobjstr
  where Right (verb, argstr) = parse command      "" $ replaceCharCommand cmd
        Right  args          = parse commandWords "" argstr
        (dobjstr, (prepSpec, prepstr), iobjstr) = matchPrep args
