
module MOO.Parser ( parse ) where

import           Text.Parsec hiding (parse)
import           Text.Parsec.Text
import           Text.Parsec.Token (GenLanguageDef(..))
import qualified Text.Parsec.Token as T
import           Text.Parsec.Error
import           Control.Monad.Identity
import           Data.Text (Text, pack, unpack, toCaseFold)
import           Data.Ratio
import           Data.Maybe
import           Data.List
import           MOO.Types
import           MOO.AST

data ParserState = ParserState { dollarContext :: Int
                               , loopStack :: [[Maybe Id]]
                               }

initParserState :: ParserState
initParserState = ParserState { dollarContext = 0
                              , loopStack     = [[]]
                              }

type MOOParser = GenParser ParserState

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
  , T.reservedNames   = ["if", "else", "elseif", "endif", "for", "in", "endfor",
                         "fork", "endfork", "return", "while", "endwhile",
                         "try", "except", "finally", "endtry", "ANY",
                         "break", "continue"] ++
                        map show ([minBound..maxBound] :: [Error])
  , T.reservedOpNames = ["+", "-", "*", "/", "%", "^",
                         "==", "!=", "<", "<=", ">=", ">", "&&", "||",
                         "?", "|", ".."]
  , T.caseSensitive   = False
  }

lexer = T.makeTokenParser mooDef

identifier     = T.identifier     lexer >>= return . pack
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

signed :: (Num a) => MOOParser a -> MOOParser a
signed parser = negative <|> parser
  where negative = char '-' >> fmap negate parser

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
        exponent = do
          char 'e' <|> char 'E' <?> "exponent"
          plusMinus decimal
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
                    fmap (Str . pack) $ many stringChar
        stringChar = noneOf "\"\\" <|> (char '\\' >> anyChar <?> "")

objectLiteral :: MOOParser Value
objectLiteral = lexeme (char '#' >> signed decimal) >>=
                return . Obj . fromIntegral
                <?> "object number"

errorLiteral :: MOOParser Value
errorLiteral = checkPrefix >> fmap Err errorValue <?> "error value"
  where checkPrefix = try $ lookAhead $ (char 'E' <|> char 'e') >> char '_'
        errorValue = choice $ map parseError [minBound..maxBound]
        parseError err = reserved (show err) >> return err

expression :: MOOParser Expr
expression = scatterAssign <|> valueOrAssign <?> "expression"
  where scatterAssign = do
          s <- try $ do
            s <- braces scatList
            symbol "="
            return s
          e <- expression
          checkScatter s
          return $ ScatterAssign s e

        valueOrAssign = do
          v <- value
          assign v <|> return v
        assign v = do
          try $ lexeme $ char '=' >> notFollowedBy (char '>')
          e <- expression
          case v of
            List [] -> fail "Empty list in scattering assignment."
            List args -> do
              s <- scatFromArgList args
              checkScatter s
              return $ ScatterAssign s e
            v | isLValue v -> return $ Assign v e
            _ -> fail "Illegal expression on left side of assignment."

value :: MOOParser Expr
value = do
  c <- conditional
  question c <|> return c
  where question cond = do
          symbol "?"
          case1 <- expression
          symbol "|"
          case2 <- conditional
          return $ Conditional cond case1 case2

conditional :: MOOParser Expr
conditional = chainl1 logical (try op)
  where op = and <|> or
        and = symbol "&&" >> return And
        or  = symbol "||" >> return Or

logical :: MOOParser Expr
logical = chainl1 relational (try op)
  where op = equal <|> notEqual <|>
             lessThan <|> lessEqual <|>
             greaterThan <|> greaterEqual <|>
             inOp
        equal        = symbol "=="   >> return Equal
        notEqual     = symbol "!="   >> return NotEqual
        lessThan     = lt            >> return LessThan
        lessEqual    = symbol "<="   >> return LessEqual
        greaterThan  = gt            >> return GreaterThan
        greaterEqual = symbol ">="   >> return GreaterEqual
        inOp         = reserved "in" >> return In

        lt = lexeme $ char '<' >> notFollowedBy (char '=')
        gt = lexeme $ char '>' >> notFollowedBy (char '=')

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
        mod    = symbol "%" >> return Mod

factor :: MOOParser Expr
factor = chainr1 base power
  where power = symbol "^" >> return Power

base :: MOOParser Expr
base = bangThing <|> minusThing <|> unary
  where bangThing = symbol "!" >> fmap Not base
        minusThing = do
          try $ lexeme $ char '-' >> notFollowedBy (digit <|> char '.')
          fmap UnaryMinus base

unary :: MOOParser Expr
unary = primary >>= modifiers

primary :: MOOParser Expr
primary = subexpression <|> dollarThing <|> identThing <|>
          list <|> catchExpr <|> literal
  where subexpression = parens expression

        dollarThing = do
          symbol "$"
          dollarRef <|> justDollar
        dollarRef = do
          name <- fmap (Literal . Str) identifier
          dollarVerb name <|> return (PropRef objectZero name)
        dollarVerb name = fmap (VerbCall objectZero name) $ parens argList
        objectZero = Literal $ Obj 0
        justDollar = do
          dc <- fmap dollarContext getState
          unless (dc > 0) $ fail "Illegal context for `$' expression."
          return Length

        identThing = do
          ident <- identifier
          let builtin = fmap (BuiltinFunc ident) $ parens argList
          builtin <|> return (Variable ident)

        list = fmap List $ braces argList

        catchExpr = do
          symbol "`"
          e <- expression
          symbol "!"
          cs <- codes
          defaultValue <- optionMaybe $ symbol "=>" >> expression
          symbol "'"
          return $ Catch e cs defaultValue

        literal = fmap Literal $ stringLiteral <|> objectLiteral <|>
                  floatLiteral <|> integerLiteral <|> errorLiteral

codes :: MOOParser Codes
codes = any <|> fmap Codes nonEmptyArgList <?> "codes"
  where any = reserved "ANY" >> return ANY

modifiers :: Expr -> MOOParser Expr
modifiers expr = (propRef  >>= modifiers) <|>
                 (verbCall >>= modifiers) <|>
                 (index    >>= modifiers) <|> return expr
  where propRef = do
          try $ dot >> notFollowedBy dot
          ref <- parens expression <|> fmap (Literal . Str) identifier
          return $ PropRef expr ref
        verbCall = do
          colon
          ref <- parens expression <|> fmap (Literal . Str) identifier
          args <- parens argList
          return $ VerbCall expr ref args
        index = between (symbol "[" >> dollars succ)
                        (symbol "]" >> dollars pred) $ do
          i <- expression
          range i <|> return (Index expr i)
        range start = do
          try $ symbol ".."
          end <- expression
          return $ Range expr start end
        dollars f = modifyState $
                    \st -> st { dollarContext = f $ dollarContext st }

nonEmptyArgList :: MOOParser [Arg]
nonEmptyArgList = arguments False

argList :: MOOParser [Arg]
argList = arguments True

arguments :: Bool -> MOOParser [Arg]
arguments allowEmpty
  | allowEmpty = commaSep  arg
  | otherwise  = commaSep1 arg
  where arg = splice <|> normal
        splice = symbol "@" >> fmap ArgSplice expression
        normal = fmap ArgNormal expression

scatList :: MOOParser [ScatItem]
scatList = commaSep1 scat
  where scat = optional <|> rest <|> required
        optional = do
          symbol "?"
          ident <- identifier
          defaultValue <- defaultExpr <|> return Nothing
          return $ ScatOptional ident defaultValue
        defaultExpr = symbol "=" >> fmap Just expression
        rest = symbol "@" >> fmap ScatRest identifier
        required = fmap ScatRequired identifier

scatFromArgList :: [Arg] -> MOOParser [ScatItem]
scatFromArgList (a:as) = do
  a' <- case a of
    ArgNormal (Variable v) -> return $ ScatRequired v
    ArgSplice (Variable v) -> return $ ScatRest v
    _ -> fail "Scattering assignment targets must be simple variables."
  as' <- scatFromArgList as
  return (a':as')
scatFromArgList [] = return []

checkScatter :: [ScatItem] -> MOOParser ()
checkScatter = checkScatter' True
  where checkScatter' restValid (s:ss) = case s of
          ScatRest{} | restValid -> checkScatter' False ss
                     | otherwise -> fail tooMany
          _ -> checkScatter' restValid ss
        checkScatter' _ [] = return ()
        tooMany = "More than one `@' target in scattering assignment."

program :: MOOParser Program
program = whiteSpace >> fmap Program statements

statements :: MOOParser [Statement]
statements = fmap catMaybes (many statement) <?> "statements"

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
  cond <- parens expression
  body <- statements
  elseIfs <- many elseIf
  elsePart <- option (Else []) $ reserved "else" >> fmap Else statements
  reserved "endif"
  return $ If cond body elseIfs elsePart
  where elseIf = do
          reserved "elseif"
          cond <- parens expression
          body <- statements
          return $ ElseIf cond body

forStatement :: MOOParser Statement
forStatement = do
  reserved "for"
  ident <- identifier
  reserved "in"
  forList ident <|> forRange ident
  where forList ident = do
          e <- parens expression
          body <- loopBody ident
          reserved "endfor"
          return $ ForList ident e body

        forRange ident = do
          (start, end) <- brackets range
          body <- loopBody ident
          reserved "endfor"
          return $ ForRange ident start end body
        range = do
          start <- expression
          symbol ".."
          end <- expression
          return (start, end)

        loopBody ident = do
          pushLoopName (Just ident)
          body <- statements
          popLoopName
          return body

whileStatement :: MOOParser Statement
whileStatement = do
  reserved "while"
  ident <- optionMaybe identifier
  e <- parens expression
  pushLoopName ident
  body <- statements
  popLoopName
  reserved "endwhile"
  return $ While ident e body

modifyLoopStack :: ([[Maybe Id]] -> [[Maybe Id]]) -> MOOParser ()
modifyLoopStack f = modifyState $ \st -> st { loopStack = f $ loopStack st }

pushLoopName :: Maybe Id -> MOOParser ()
pushLoopName ident = modifyLoopStack $ \(s:ss) -> (fmap toCaseFold ident:s):ss

popLoopName :: MOOParser ()
popLoopName = modifyLoopStack $ \(s:ss) -> tail s : ss

suspendLoopScope :: MOOParser ()
suspendLoopScope = modifyLoopStack $ \ss -> [] : ss

resumeLoopScope :: MOOParser ()
resumeLoopScope = modifyLoopStack tail

breakStatement :: MOOParser Statement
breakStatement = do
  reserved "break"
  ident <- optionMaybe identifier
  semi
  checkLoopName "break" ident
  return $ Break ident

continueStatement :: MOOParser Statement
continueStatement = do
  reserved "continue"
  ident <- optionMaybe identifier
  semi
  checkLoopName "continue" ident
  return $ Continue ident

checkLoopName :: String -> Maybe Id -> MOOParser ()
checkLoopName kind ident = do
  stack <- fmap (head . loopStack) getState
  case ident of
    Nothing -> when (null stack) $
               fail $ "No enclosing loop for `" ++ kind ++ "' statement"
    Just name -> when (fmap toCaseFold ident `notElem` stack) $
                 fail $ "Invalid loop name in `" ++ kind ++ "' statement: " ++
                 unpack name

returnStatement :: MOOParser Statement
returnStatement = do
  reserved "return"
  e <- optionMaybe expression
  semi
  return $ Return e

tryStatement :: MOOParser Statement
tryStatement = do
  reserved "try"
  body <- statements
  tryExcept body <|> tryFinally body
  where tryExcept body = do
          excepts <- many1 except
          reserved "endtry"
          return $ TryExcept body excepts
        except = do
          reserved "except"
          ident <- optionMaybe identifier
          cs <- parens codes
          handler <- statements
          return $ Except ident cs handler

        tryFinally body = do
          reserved "finally"
          cleanup <- statements
          reserved "endtry"
          return $ TryFinally body cleanup

forkStatement :: MOOParser Statement
forkStatement = do
  reserved "fork"
  ident <- optionMaybe identifier
  e <- parens expression
  suspendLoopScope
  body <- statements
  resumeLoopScope
  reserved "endfork"
  return $ Fork ident e body

expressionStatement :: MOOParser Statement
expressionStatement = do
  e <- expression
  semi
  return $ Expression e

type Errors = [Text]

parse :: Text -> Either Errors Program
parse input = case runParser program initParserState "MOO code" input of
  Right p -> Right p
  Left err -> Left $ let line = sourceLine $ errorPos err
                         msg = find message $ errorMessages err
                         message Message{} = True
                         message _         = False
                     in [pack $ "Line " ++ show line ++ ":  " ++
                         maybe "syntax error" messageString msg]

-- Testing ...

test :: (Show a) => MOOParser a -> Text -> IO ()
test p input = case runParser p initParserState "test" input of
  Left err -> print err
  Right v  -> print v
