
{-# LANGUAGE OverloadedStrings #-}

-- | Recovering MOO code from an abstract syntax tree for the @verb_code()@
-- built-in function
module MOO.Unparser ( unparse ) where

import Control.Monad (when, unless, liftM, (<=<))
import Control.Monad.Reader (ReaderT, runReaderT, asks, local)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Char (isAlpha, isAlphaNum)
import Data.List (intersperse)
import Data.Monoid ((<>), mconcat)
import Data.Set (Set)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder, toLazyText, fromText)

import qualified Data.Set as S
import qualified Data.Text as T

import MOO.AST
import MOO.Types
import MOO.Parser (keywords)

type Unparser = ReaderT UnparserEnv (Writer Builder)

data UnparserEnv = UnparserEnv {
    fullyParenthesizing :: Bool
  , indenting           :: Bool
  , indentation         :: Builder
}

initUnparserEnv = UnparserEnv {
    fullyParenthesizing = False
  , indenting           = False
  , indentation         = ""
}

-- | Synthesize the MOO code corresponding to the given abstract syntax
-- tree. If /fully-paren/ is true, all expressions are fully parenthesized,
-- even when unnecessary given the circumstances of precedence. If /indent/ is
-- true, the resulting MOO code will be indented with spaces as appropriate to
-- show the nesting structure of statements.
--
-- The MOO code is returned as a single 'Text' value containing embedded
-- newline characters.
unparse :: Bool     -- ^ /fully-paren/
        -> Bool     -- ^ /indent/
        -> Program
        -> Text
unparse fullyParen indent (Program stmts) =
  toLazyText $ execWriter $ runReaderT (tellStatements stmts) $
  initUnparserEnv {
      fullyParenthesizing = fullyParen
    , indenting           = indent
  }

indent :: Unparser ()
indent = do
  indenting <- asks indenting
  when indenting $ tell =<< asks indentation

moreIndented :: Unparser a -> Unparser a
moreIndented = local $ \env -> env { indentation = "  " <> indentation env }

tellStatements :: [Statement] -> Unparser ()
tellStatements = mapM_ tellStatement

tellStatement :: Statement -> Unparser ()
tellStatement stmt = case stmt of
  Expression _ expr -> indent >> tellExpr expr >> tell ";\n"

  If _ cond (Then thens) elseIfs (Else elses) -> do
    indent >> tell "if (" >> tellExpr cond >> tell ")\n"
    moreIndented $ tellStatements thens
    mapM_ tellElseIf elseIfs
    unless (null elses) $ do
      indent >> tell "else\n"
      moreIndented $ tellStatements elses
    indent >> tell "endif\n"

    where tellElseIf (ElseIf _ cond thens) = do
            indent >> tell "elseif (" >> tellExpr cond >> tell ")\n"
            moreIndented $ tellStatements thens

  ForList _ var expr body -> tellBlock "for" (Just var) detail body
    where detail = tell " in" >> detailExpr expr

  ForRange _ var (start, end) body -> tellBlock "for" (Just var) detail body
    where detail = tell " in [" >> tellExpr start >> tell ".." >>
                                   tellExpr end   >> tell "]\n"

  While _ var expr body -> tellBlock "while" var (detailExpr expr) body
  Fork  _ var expr body -> tellBlock "fork"  var (detailExpr expr) body

  Break    var -> indent >> tell "break"    >> maybeTellVar var >> tell ";\n"
  Continue var -> indent >> tell "continue" >> maybeTellVar var >> tell ";\n"

  Return _ Nothing     -> indent >> tell "return;\n"
  Return _ (Just expr) -> indent >> tell "return " >>
                          tellExpr expr >> tell ";\n"

  TryExcept body excepts -> do
    indent >> tell "try\n"
    moreIndented $ tellStatements body
    mapM_ tellExcept excepts
    indent >> tell "endtry\n"

    where tellExcept (Except _ var codes handler) = do
            indent >> tell "except" >> maybeTellVar var >> tell " ("
            case codes of
              ANY        -> tell "ANY"
              Codes args -> tell =<< unparseArgs args
            tell ")\n"
            moreIndented $ tellStatements handler

  TryFinally body (Finally finally) -> do
    indent >> tell "try\n"
    moreIndented $ tellStatements body
    indent >> tell "finally\n"
    moreIndented $ tellStatements finally
    indent >> tell "endtry\n"

tellBlock :: StrT -> Maybe Id -> Unparser () -> [Statement] -> Unparser ()
tellBlock name maybeVar detail body = do
  indent >> tell name' >> maybeTellVar maybeVar >> detail
  moreIndented $ tellStatements body
  indent >> tell "end" >> tell name' >> tell "\n"
  where name' = fromText name

maybeTellVar :: Maybe Id -> Unparser ()
maybeTellVar Nothing    = return ()
maybeTellVar (Just var) = tell " " >> tell (fromText var)

detailExpr :: Expr -> Unparser ()
detailExpr expr = tell " (" >> tellExpr expr >> tell ")\n"

tellExpr :: Expr -> Unparser ()
tellExpr = tell <=< unparseExpr

unparseExpr :: Expr -> Unparser Builder
unparseExpr expr = case expr of
  Literal value -> return (fromText $ toLiteral value)

  List args -> do
    args' <- unparseArgs args
    return $ "{" <> args' <> "}"

  Variable var -> return (fromText var)

  PropRef (Literal (Obj 0)) (Literal (Str name))
    | isIdentifier name -> return $ "$" <> fromText name
  PropRef obj name -> do
    obj' <- case obj of
      Literal Int{} -> paren obj  -- avoid digits followed by dot (-> float)
      _             -> parenL expr obj
    name' <- unparseNameExpr name
    return $ obj' <> "." <> name'

  Assign lhs rhs -> do
    lhs' <- unparseExpr lhs
    rhs' <- unparseExpr rhs
    return $ lhs' <> " = " <> rhs'

  ScatterAssign scats rhs -> do
    scats' <- unparseScatter scats
    rhs' <- unparseExpr rhs
    return $ "{" <> scats' <> "} = " <> rhs'

  VerbCall (Literal (Obj 0)) (Literal (Str name)) args
    | isIdentifier name -> do args' <- unparseArgs args
                              return $ "$" <> fromText name <>
                                       "(" <> args' <> ")"
  VerbCall obj name args -> do
    obj' <- parenL expr obj
    name' <- unparseNameExpr name
    args' <- unparseArgs args
    return $ obj' <> ":" <> name' <> "(" <> args' <> ")"

  BuiltinFunc func args -> do
    args' <- unparseArgs args
    return $ fromText func <> "(" <> args' <> ")"

  Index lhs rhs -> do
    lhs' <- parenL expr lhs
    rhs' <- unparseExpr rhs
    return $ lhs' <> "[" <> rhs' <> "]"

  Range lhs (from, to) -> do
    lhs'  <- parenL expr lhs
    from' <- unparseExpr from
    to'   <- unparseExpr to
    return $ lhs' <> "[" <> from' <> ".." <> to' <> "]"

  Length -> return "$"

  -- Left-associative operators
  In           lhs rhs -> binaryL lhs " in " rhs
  Plus         lhs rhs -> binaryL lhs " + "  rhs
  Minus        lhs rhs -> binaryL lhs " - "  rhs
  Times        lhs rhs -> binaryL lhs " * "  rhs
  Divide       lhs rhs -> binaryL lhs " / "  rhs
  Remain       lhs rhs -> binaryL lhs " % "  rhs
  And          lhs rhs -> binaryL lhs " && " rhs
  Or           lhs rhs -> binaryL lhs " || " rhs
  Equal        lhs rhs -> binaryL lhs " == " rhs
  NotEqual     lhs rhs -> binaryL lhs " != " rhs
  LessThan     lhs rhs -> binaryL lhs " < "  rhs
  LessEqual    lhs rhs -> binaryL lhs " <= " rhs
  GreaterThan  lhs rhs -> binaryL lhs " > "  rhs
  GreaterEqual lhs rhs -> binaryL lhs " >= " rhs

  -- Right-associative operators
  Power        lhs rhs -> binaryR lhs " ^ "  rhs

  Negate lhs@(Literal x)                | numeric x -> negateParen lhs
  Negate lhs@(Literal x `Index` _)      | numeric x -> negateParen lhs
  Negate lhs@(Literal x `Range` _)      | numeric x -> negateParen lhs
  Negate lhs@(Literal Flt{} `PropRef` _)            -> negateParen lhs
  Negate lhs@(VerbCall (Literal x) _ _) | numeric x -> negateParen lhs
  Negate lhs -> ("-" <>) `liftM` parenL expr lhs

  Not lhs -> ("!" <>) `liftM` parenL expr lhs

  Conditional cond lhs rhs -> do
    cond' <- parenR expr cond
    lhs'  <- unparseExpr lhs
    rhs'  <- parenR expr rhs
    return $ cond' <> " ? " <> lhs' <> " | " <> rhs'

  Catch lhs codes (Default dv) -> do
    lhs' <- unparseExpr lhs
    codes' <- case codes of
      ANY        -> return "ANY"
      Codes args -> unparseArgs args
    case dv of
      Nothing   -> return $ "`" <> lhs' <> " ! " <> codes' <> "'"
      Just expr -> do
        expr' <- unparseExpr expr
        return $ "`" <> lhs' <> " ! " <> codes' <> " => " <> expr' <> "'"

  where binaryL lhs op rhs = do
          lhs' <- parenL expr lhs
          rhs' <- parenR expr rhs
          return $ lhs' <> op <> rhs'

        binaryR lhs op rhs = do
          lhs' <- parenR expr lhs
          rhs' <- parenL expr rhs
          return $ lhs' <> op <> rhs'

        numeric :: Value -> Bool
        numeric Int{} = True
        numeric Flt{} = True
        numeric _     = False

        negateParen = liftM ("-" <>) . paren

unparseArgs :: [Arg] -> Unparser Builder
unparseArgs = liftM (mconcat . intersperse ", ") . mapM unparseArg
  where unparseArg (ArgNormal expr) =                  unparseExpr expr
        unparseArg (ArgSplice expr) = ("@" <>) `liftM` unparseExpr expr

unparseScatter :: [ScatItem] -> Unparser Builder
unparseScatter = liftM (mconcat . intersperse ", ") . mapM unparseScat
  where unparseScat (ScatRequired var)         = return $        fromText var
        unparseScat (ScatRest     var)         = return $ "@" <> fromText var
        unparseScat (ScatOptional var Nothing) = return $ "?" <> fromText var
        unparseScat (ScatOptional var (Just expr)) = do
          expr' <- unparseExpr expr
          return $ "?" <> fromText var <> " = " <> expr'

unparseNameExpr :: Expr -> Unparser Builder
unparseNameExpr (Literal (Str name))
  | isIdentifier name = return (fromText name)
unparseNameExpr expr = paren expr

paren :: Expr -> Unparser Builder
paren expr = do
  expr' <- unparseExpr expr
  return $ "(" <> expr' <> ")"

mightParen :: (Int -> Int -> Bool) -> Expr -> Expr -> Unparser Builder
mightParen cmp parent child = do
  fullyParenthesizing <- asks fullyParenthesizing
  if (fullyParenthesizing && precedence child < precedence PropRef{}) ||
     (precedence parent `cmp` precedence child)
    then paren child
    else unparseExpr child

parenL :: Expr -> Expr -> Unparser Builder
parenL = mightParen (>)

parenR :: Expr -> Expr -> Unparser Builder
parenR = mightParen (>=)

isIdentifier :: StrT -> Bool
isIdentifier name = isIdentifier' (T.unpack name) && not (isKeyword name)
  where isIdentifier' (c:cs) = isIdentStart c && all isIdentChar cs
        isIdentStart   c     = isAlpha    c || c == '_'
        isIdentChar    c     = isAlphaNum c || c == '_'

isKeyword :: StrT -> Bool
isKeyword = (`S.member` keywordsSet) . T.toCaseFold

keywordsSet :: Set StrT
keywordsSet = S.fromList $ map (T.toCaseFold . T.pack) keywords
