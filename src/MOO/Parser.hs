
module MOO.Parser ( Program, parse, runParser, initParserState
                  , expression, between, whiteSpace, eof, program
                  , parseInt, parseFlt, parseNum, parseObj, keywords ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, unless, mplus)
import Control.Monad.Identity (Identity)
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Ratio ((%))
import Data.String (fromString)
import Data.Text (Text)
import Text.Parsec (try, many, many1, digit, letter, char, anyChar, alphaNum,
                    oneOf, noneOf, lookAhead, notFollowedBy, chainl1, chainr1,
                    option, optionMaybe, choice, between, getState, modifyState,
                    eof, runParser, sourceLine, errorPos, (<|>), (<?>))
import Text.Parsec.Error (Message(Message), errorMessages, messageString)
import Text.Parsec.Text (GenParser)
import Text.Parsec.Token (GenLanguageDef(LanguageDef))

import qualified Text.Parsec.Token as T

import MOO.AST
import MOO.Types

data ParserState = ParserState {
    dollarContext :: Int
  , loopStack     :: [[Maybe Id]]
  , lineNumber    :: Int
}

initParserState = ParserState {
    dollarContext = 0
  , loopStack     = [[]]
  , lineNumber    = 1
}

type MOOParser = GenParser ParserState

keywords :: [String]
keywords = ["if", "elseif", "else", "endif", "for", "in", "endfor",
            "while", "endwhile", "fork", "endfork", "return",
            "try", "except", "finally", "endtry", "ANY",
            "break", "continue"] ++ map show ([minBound..maxBound] :: [Error])

mooDef :: GenLanguageDef Text u Identity
mooDef = LanguageDef {
    T.commentStart    = "/*"
  , T.commentEnd      = "*/"
  , T.commentLine     = ""
  , T.nestedComments  = False
  , T.identStart      = letter   <|> char '_'
  , T.identLetter     = alphaNum <|> char '_'
  , T.opStart         = T.opLetter mooDef
  , T.opLetter        = oneOf "+-*/%^=!<>?&|."
  , T.reservedNames   = keywords
  , T.reservedOpNames = ["+", "-", "*", "/", "%", "^",
                         "==", "!=", "<", "<=", ">=", ">", "&&", "||",
                         "?", "|", ".."]
  , T.caseSensitive   = False
  }

lexer = T.makeTokenParser mooDef

{-# ANN identifier ("HLint: ignore Use liftM" :: String) #-}

identifier     = T.identifier     lexer >>= return . fromString
reserved       = T.reserved       lexer
decimal        = T.decimal        lexer
symbol         = T.symbol         lexer
lexeme         = T.lexeme         lexer
whiteSpace     = T.whiteSpace     lexer
parens         = T.parens         lexer
braces         = T.braces         lexer
brackets       = T.brackets       lexer
semi           = T.semi           lexer
colon          = T.colon          lexer
dot            = T.dot            lexer
commaSep       = T.commaSep       lexer
commaSep1      = T.commaSep1      lexer

{-
operator       = T.operator       lexer
reservedOp     = T.reservedOp     lexer
charLiteral    = T.charLiteral    lexer
stringLiteral  = T.stringLiteral  lexer
natural        = T.natural        lexer
integer        = T.integer        lexer
float          = T.float          lexer
naturalOrFloat = T.naturalOrFloat lexer
hexadecimal    = T.hexadecimal    lexer
octal          = T.octal          lexer
angles         = T.angles         lexer
comma          = T.comma          lexer
semiSep        = T.semiSep        lexer
semiSep1       = T.semiSep1       lexer
-}

-- Literal values

signed :: (Num a) => MOOParser a -> MOOParser a
signed parser = negative <|> parser
  where negative = char '-' >> negate <$> parser

plusMinus :: (Num a) => MOOParser a -> MOOParser a
plusMinus parser = positive <|> signed parser
  where positive = char '+' >> parser

integerLiteral :: MOOParser Value
integerLiteral = try (lexeme $ signed decimal) >>= return . Int . fromIntegral
                 <?> "integer literal"

floatLiteral :: MOOParser Value
floatLiteral = try (lexeme $ signed real) >>= checkRange >>= return . Flt
               <?> "floating-point literal"
  where real = try withDot <|> withoutDot
        withDot = do
          pre <- many digit
          char '.' >> notFollowedBy (char '.')
          post <- (if null pre then many1 else many) digit
          exp <- optionMaybe exponent
          mkFloat pre post exp
        withoutDot = do
          pre <- many1 digit
          exp <- exponent
          mkFloat pre "" (Just exp)

        exponent = oneOf "eE" >> plusMinus decimal <?> "exponent"

        mkFloat pre post exp =
          let whole = if null pre  then 0 else read pre  % 1
              frac  = if null post then 0 else read post % (10 ^ length post)
              mantissa = whole + frac
          in return $ case exp of
            Nothing -> fromRational mantissa
            Just e | e < -500 ||
                     e >  500  -> fromRational   mantissa *      (10 ^^  e)
                   | e <    0  -> fromRational $ mantissa * (1 % (10 ^ (-e)))
                   | otherwise -> fromRational $ mantissa *      (10 ^   e)

        checkRange flt =
          if isInfinite flt
          then fail "Floating-point literal out of range"
          else return flt

stringLiteral :: MOOParser Value
stringLiteral = lexeme mooString <?> "string literal"
  where mooString = between (char '"') (char '"' <?> "terminating quote") $
                    Str . fromString <$> many stringChar
        stringChar = noneOf "\"\\" <|> (char '\\' >> anyChar <?> "")

objectLiteral :: MOOParser Value
objectLiteral = lexeme (char '#' >> signed decimal) >>=
                return . Obj . fromIntegral
                <?> "object number"

errorLiteral :: MOOParser Value
errorLiteral = checkPrefix >> Err <$> errorValue <?> "error value"
  where checkPrefix = try $ lookAhead $ (char 'E' <|> char 'e') >> char '_'
        errorValue = choice $ map literal [minBound..maxBound]
        literal err = reserved (show err) >> return err

-- Expressions

expression :: MOOParser Expr
expression = scatterAssign <|> valueOrAssign <?> "expression"
  where scatterAssign = do
          scat <- try $ do
            s <- braces scatList
            lexeme $ char '=' >> notFollowedBy (oneOf "=>")
            return s
          expr <- expression
          mkScatter scat expr

        valueOrAssign = do
          val <- value
          assign val <|> return val
        assign val = do
          try $ lexeme $ char '=' >> notFollowedBy (oneOf "=>")
          expr <- expression
          case val of
            List args -> do
              scat <- scatFromArgList args
              mkScatter scat expr
            val | isLValue val -> return $ Assign val expr
            _ -> fail "Illegal expression on left side of assignment."

value :: MOOParser Expr
value = do
  cond <- conditional
  question cond <|> return cond
  where question cond = do
          symbol "?"
          t <- expression
          symbol "|"
          f <- conditional
          return $ Conditional cond t f

conditional :: MOOParser Expr
conditional = chainl1 logical (try op)
  where op = and <|> or
        and = symbol "&&" >> return And
        or  = symbol "||" >> return Or

logical :: MOOParser Expr
logical = chainl1 relational (try op)
  where op = equal <|> notEqual <|> lessThan <|> lessEqual <|>
             greaterThan <|> greaterEqual <|> inOp
        equal        = symbol "=="   >> return CompareEQ
        notEqual     = symbol "!="   >> return CompareNE
        lessThan     = lt            >> return CompareLT
        lessEqual    = symbol "<="   >> return CompareLE
        greaterThan  = gt            >> return CompareGT
        greaterEqual = symbol ">="   >> return CompareGE
        inOp         = reserved "in" >> return In

        lt = try $ lexeme $ char '<' >> notFollowedBy (char '=')
        gt = try $ lexeme $ char '>' >> notFollowedBy (char '=')

relational :: MOOParser Expr
relational = chainl1 term (try op)
  where op = plus <|> minus
        plus  = symbol "+" >> return Plus
        minus = symbol "-" >> return Minus

term :: MOOParser Expr
term = chainl1 factor (try op)
  where op = times <|> divide <|> mod
        times  = symbol "*" >> return Times
        divide = symbol "/" >> return Divide
        mod    = symbol "%" >> return Remain

factor :: MOOParser Expr
factor = chainr1 base power
  where power = symbol "^" >> return Power

base :: MOOParser Expr
base = bangThing <|> minusThing <|> unary
  where bangThing = symbol "!" >> Not <$> base
        minusThing = do
          try $ lexeme $ char '-' >> notFollowedBy (digit <|> char '.')
          Negate <$> base

unary :: MOOParser Expr
unary = primary >>= modifiers

primary :: MOOParser Expr
primary = subexpression <|> dollarThing <|> identThing <|>
          list <|> catchExpr <|> literal
  where subexpression = parens expression

        dollarThing = symbol "$" >> (dollarRef <|> justDollar)
        dollarRef = do
          name <- Literal . Str . fromId <$> identifier
          dollarVerb name <|> return (PropertyRef objectZero name)
        dollarVerb name = VerbCall objectZero name <$> parens argList
        objectZero = Literal (Obj 0)
        justDollar = do
          dc <- dollarContext <$> getState
          unless (dc > 0) $ fail "Illegal context for `$' expression."
          return Length

        identThing = do
          ident <- identifier
          let builtin = BuiltinFunc ident <$> parens argList
          builtin <|> return (Variable ident)

        list = List <$> braces argList

        catchExpr = do
          symbol "`"
          expr <- expression
          symbol "!"
          cs <- codes
          dv <- optionMaybe $ symbol "=>" >> expression
          symbol "'"
          return $ Catch expr cs (Default dv)

        literal = fmap Literal $ stringLiteral <|> objectLiteral <|>
                  floatLiteral <|> integerLiteral <|> errorLiteral

modifiers :: Expr -> MOOParser Expr
modifiers expr = (propRef  >>= modifiers) <|>
                 (verbCall >>= modifiers) <|>
                 (index    >>= modifiers) <|> return expr
  where propRef = do
          try $ dot >> notFollowedBy dot
          ref <- parens expression <|> fmap (Literal . Str . fromId) identifier
          return $ PropertyRef expr ref
        verbCall = do
          colon
          ref <- parens expression <|> fmap (Literal . Str . fromId) identifier
          args <- parens argList
          return $ VerbCall expr ref args
        index = between (symbol "[" >> dollars succ)
                        (symbol "]" >> dollars pred) $ do
          i <- expression
          range i <|> return (Index expr i)
        range start = do
          try $ symbol ".."
          end <- expression
          return $ Range expr (start, end)
        dollars f = modifyState $
                    \st -> st { dollarContext = f $ dollarContext st }

codes :: MOOParser Codes
codes = any <|> fmap Codes nonEmptyArgList <?> "codes"
  where any = reserved "ANY" >> return ANY

nonEmptyArgList :: MOOParser [Argument]
nonEmptyArgList = arguments False

argList :: MOOParser [Argument]
argList = arguments True

arguments :: Bool -> MOOParser [Argument]
arguments allowEmpty
  | allowEmpty = commaSep  arg
  | otherwise  = commaSep1 arg
  where arg = splice <|> normal
        splice = symbol "@" >> ArgSplice <$> expression
        normal =               ArgNormal <$> expression

scatList :: MOOParser [ScatterItem]
scatList = commaSep1 scat
  where scat = optional <|> rest <|> required
        optional = symbol "?" >> ScatOptional <$> identifier <*>
                   optionMaybe (symbol "=" >> expression)
        rest     = symbol "@" >> ScatRest     <$> identifier
        required =               ScatRequired <$> identifier

scatFromArgList :: [Argument] -> MOOParser [ScatterItem]
scatFromArgList [] = fail "Empty list in scattering assignment."
scatFromArgList args = go args
  where go (a:as) = do
          a' <- case a of
            ArgNormal (Variable v) -> return $ ScatRequired v
            ArgSplice (Variable v) -> return $ ScatRest v
            _ -> fail "Scattering assignment targets must be simple variables."
          as' <- go as
          return (a':as')
        go [] = return []

mkScatter :: [ScatterItem] -> Expr -> MOOParser Expr
mkScatter scat expr = checkScatter True scat
  where checkScatter restValid (s:ss) = case s of
          ScatRest{} | restValid -> checkScatter False ss
                     | otherwise -> fail tooMany
          _ -> checkScatter restValid ss
        checkScatter _ [] = return $ Scatter scat expr
        tooMany = "More than one `@' target in scattering assignment."

-- Statements

incLineNumber :: MOOParser ()
incLineNumber = modifyState $ \st -> st { lineNumber = succ (lineNumber st) }

getLineNumber :: MOOParser Int
getLineNumber = lineNumber <$> getState

statements :: MOOParser [Statement]
statements = catMaybes <$> many statement <?> "statements"

statement :: MOOParser (Maybe Statement)
statement = fmap Just someStatement <|> nullStatement <?> "statement"
  where someStatement = ifStatement <|> forStatement <|> whileStatement <|>
                        breakStatement <|> continueStatement <|>
                        returnStatement <|> tryStatement <|> forkStatement <|>
                        expressionStatement
        nullStatement = semi >> return Nothing

ifStatement :: MOOParser Statement
ifStatement = do
  reserved "if"
  (lineNumber, cond, body) <- ifThen
  elseIfs <- many elseIf
  elsePart <- option [] $ reserved "else" >> incLineNumber >> statements
  reserved "endif" >> incLineNumber

  return $ If lineNumber cond (Then body) elseIfs (Else elsePart)

  where ifThen = do
          lineNumber <- getLineNumber
          cond <- parens expression
          incLineNumber
          body <- statements
          return (lineNumber, cond, body)

        elseIf = do
          reserved "elseif"
          (lineNumber, cond, body) <- ifThen
          return $ ElseIf lineNumber cond body

forStatement :: MOOParser Statement
forStatement = do
  reserved "for"
  lineNumber <- getLineNumber
  ident <- identifier
  reserved "in"
  forList lineNumber ident <|> forRange lineNumber ident
  where forList lineNumber ident = do
          expr <- parens expression
          body <- forBody ident
          return $ ForList lineNumber ident expr body

        forRange lineNumber ident = do
          range <- brackets $ do
            start <- expression
            symbol ".."
            end <- expression
            return (start, end)
          body <- forBody ident
          return $ ForRange lineNumber ident range body

        forBody ident = do
          incLineNumber
          body <- between (pushLoopName $ Just ident) popLoopName statements
          reserved "endfor" >> incLineNumber
          return body

blockStart :: MOOParser (Int, Maybe Id, Expr)
blockStart = do
  lineNumber <- getLineNumber
  ident <- optionMaybe identifier
  expr <- parens expression
  incLineNumber
  return (lineNumber, ident, expr)

whileStatement :: MOOParser Statement
whileStatement = do
  reserved "while"
  (lineNumber, ident, expr) <- blockStart
  body <- between (pushLoopName ident) popLoopName statements
  reserved "endwhile" >> incLineNumber
  return $ While lineNumber ident expr body

modifyLoopStack :: ([[Maybe Id]] -> [[Maybe Id]]) -> MOOParser ()
modifyLoopStack f = modifyState $ \st -> st { loopStack = f $ loopStack st }

pushLoopName :: Maybe Id -> MOOParser ()
pushLoopName ident = modifyLoopStack $ \(s:ss) -> (ident : s) : ss

popLoopName :: MOOParser ()
popLoopName = modifyLoopStack $ \(s:ss) -> tail s : ss

suspendLoopScope :: MOOParser ()
suspendLoopScope = modifyLoopStack $ \ss -> [] : ss

resumeLoopScope :: MOOParser ()
resumeLoopScope = modifyLoopStack tail

checkLoopName :: String -> Maybe Id -> MOOParser ()
checkLoopName kind ident = do
  stack <- head . loopStack <$> getState
  case ident of
    Nothing -> when (null stack) $
               fail $ "No enclosing loop for `" ++ kind ++ "' statement"
    Just name -> when (ident `notElem` stack) $
                 fail $ "Invalid loop name in `" ++ kind ++ "' statement: " ++
                 fromId name

breakStatement :: MOOParser Statement
breakStatement = do
  reserved "break"
  ident <- optionMaybe identifier
  checkLoopName "break" ident
  semi >> incLineNumber >> return (Break ident)

continueStatement :: MOOParser Statement
continueStatement = do
  reserved "continue"
  ident <- optionMaybe identifier
  checkLoopName "continue" ident
  semi >> incLineNumber >> return (Continue ident)

returnStatement :: MOOParser Statement
returnStatement = do
  reserved "return"
  lineNumber <- getLineNumber
  expr <- optionMaybe expression
  semi >> incLineNumber >> return (Return lineNumber expr)

tryStatement :: MOOParser Statement
tryStatement = do
  reserved "try"
  incLineNumber
  body <- statements
  tryExcept body <|> tryFinally body
  where tryExcept body = do
          excepts <- many1 except
          reserved "endtry" >> incLineNumber
          return $ TryExcept body excepts
        except = do
          reserved "except"
          lineNumber <- getLineNumber
          ident <- optionMaybe identifier
          cs <- parens codes
          incLineNumber
          handler <- statements
          return $ Except lineNumber ident cs handler

        tryFinally body = do
          reserved "finally" >> incLineNumber
          cleanup <- statements
          reserved "endtry" >> incLineNumber
          return $ TryFinally body (Finally cleanup)

forkStatement :: MOOParser Statement
forkStatement = do
  reserved "fork"
  (lineNumber, ident, expr) <- blockStart
  body <- between suspendLoopScope resumeLoopScope statements
  reserved "endfork" >> incLineNumber
  return $ Fork lineNumber ident expr body

expressionStatement :: MOOParser Statement
expressionStatement = do
  lineNumber <- getLineNumber
  expr <- expression
  semi >> incLineNumber >> return (Expression lineNumber expr)

-- Main parser interface

program :: MOOParser Program
program = between whiteSpace eof $ Program <$> statements

type Errors = [String]

parse :: Text -> Either Errors Program
parse input = case runParser program initParserState "MOO code" input of
  Right prog -> Right prog
  Left err   -> Left $ let line = sourceLine $ errorPos err
                           msg = find message $ errorMessages err
                           message Message{} = True
                           message _         = False
                       in ["Line " ++ show line ++ ":  " ++
                           maybe "syntax error" messageString msg]

-- Auxiliary parser interface

standalone :: MOOParser Value -> Text -> Maybe Value
standalone parser input = either (const Nothing) Just $
                          runParser parser' initParserState "" input
  where parser' = between whiteSpace eof parser

parseInt :: Text -> Maybe Value
parseInt = standalone integerLiteral

parseFlt :: Text -> Maybe Value
parseFlt = standalone floatLiteral

parseNum :: Text -> Maybe Value
parseNum str = parseInt str `mplus` parseFlt str

parseObj :: Text -> Maybe Value
parseObj = standalone objectLiteral
