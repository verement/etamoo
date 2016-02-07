
{-# LANGUAGE OverloadedStrings #-}

-- | Recovering MOO code from an abstract syntax tree for the @verb_code()@
-- built-in function
module MOO.Unparser (unparse) where

import Control.Applicative ((<$>))
import Control.Monad (unless, (<=<))
import Control.Monad.Reader (ReaderT, runReaderT, asks, local)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Char (isAlpha, isAlphaNum)
import Data.HashSet (HashSet)
import Data.List (intersperse)
import Data.Monoid ((<>), mconcat)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (Builder, toLazyText)

import qualified Data.HashSet as HS

import MOO.AST
import MOO.Parser (keywords)
import MOO.Types

import qualified MOO.String as Str

type Unparser = ReaderT UnparserEnv (Writer Builder)

data UnparserEnv = UnparserEnv {
    fullyParenthesizing :: Bool
  , indentation         :: Maybe Builder
}

-- | Synthesize the MOO code corresponding to the given abstract syntax
-- tree. If /fully-paren/ is true, all expressions are fully parenthesized,
-- even when unnecessary given the circumstances of precedence. If /indent/ is
-- true, the resulting MOO code will be indented with spaces as appropriate to
-- show the nesting structure of statements.
--
-- The MOO code is returned as a single lazy 'Text' value containing embedded
-- newline characters.
unparse :: Bool     -- ^ /fully-paren/
        -> Bool     -- ^ /indent/
        -> Program
        -> Text
unparse fullyParen indent (Program stmts) =
  toLazyText $ execWriter $ runReaderT (tellStatements stmts) UnparserEnv {
    fullyParenthesizing = fullyParen
  , indentation         = if indent then Just "" else Nothing
  }

indent :: Unparser ()
indent = maybe (return ()) tell =<< asks indentation

moreIndented :: Unparser a -> Unparser a
moreIndented = local $ \env ->
  env { indentation = ("  " <>) <$> indentation env }

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

tellBlock :: Builder -> Maybe Id -> Unparser () -> [Statement] -> Unparser ()
tellBlock name maybeVar detail body = do
  indent >> tell name >> maybeTellVar maybeVar >> detail
  moreIndented $ tellStatements body
  indent >> tell "end" >> tell name >> tell "\n"

maybeTellVar :: Maybe Id -> Unparser ()
maybeTellVar Nothing    = return ()
maybeTellVar (Just var) = tell " " >> tell (fromId var)

detailExpr :: Expr -> Unparser ()
detailExpr expr = tell " (" >> tellExpr expr >> tell ")\n"

tellExpr :: Expr -> Unparser ()
tellExpr = tell <=< unparseExpr

unparseExpr :: Expr -> Unparser Builder
unparseExpr expr = case expr of
  Literal value -> return (toBuilder' value)

  List args -> do
    args' <- unparseArgs args
    return $ "{" <> args' <> "}"

  Variable var -> return (fromId var)

  PropertyRef (Literal (Obj 0)) (Literal (Str name))
    | isIdentifier name -> return $ "$" <> Str.toBuilder name
  PropertyRef obj name -> do
    obj' <- case obj of
      Literal Int{} -> paren obj  -- avoid digits followed by dot (-> float)
      _             -> parenL expr obj
    name' <- unparseNameExpr name
    return $ obj' <> "." <> name'

  Assign lhs rhs -> do
    lhs' <- unparseExpr lhs
    rhs' <- unparseExpr rhs
    return $ lhs' <> " = " <> rhs'

  Scatter scats rhs -> do
    scats' <- unparseScatter scats
    rhs' <- unparseExpr rhs
    return $ "{" <> scats' <> "} = " <> rhs'

  VerbCall (Literal (Obj 0)) (Literal (Str name)) args
    | isIdentifier name -> do args' <- unparseArgs args
                              return $ "$" <> Str.toBuilder name <>
                                       "(" <> args' <> ")"
  VerbCall obj name args -> do
    obj' <- parenL expr obj
    name' <- unparseNameExpr name
    args' <- unparseArgs args
    return $ obj' <> ":" <> name' <> "(" <> args' <> ")"

  BuiltinFunc func args -> do
    args' <- unparseArgs args
    return $ fromId func <> "(" <> args' <> ")"

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
  In        lhs rhs -> binaryL lhs " in " rhs
  Plus      lhs rhs -> binaryL lhs " + "  rhs
  Minus     lhs rhs -> binaryL lhs " - "  rhs
  Times     lhs rhs -> binaryL lhs " * "  rhs
  Divide    lhs rhs -> binaryL lhs " / "  rhs
  Remain    lhs rhs -> binaryL lhs " % "  rhs
  And       lhs rhs -> binaryL lhs " && " rhs
  Or        lhs rhs -> binaryL lhs " || " rhs
  CompareEQ lhs rhs -> binaryL lhs " == " rhs
  CompareNE lhs rhs -> binaryL lhs " != " rhs
  CompareLT lhs rhs -> binaryL lhs " < "  rhs
  CompareLE lhs rhs -> binaryL lhs " <= " rhs
  CompareGT lhs rhs -> binaryL lhs " > "  rhs
  CompareGE lhs rhs -> binaryL lhs " >= " rhs

  -- Right-associative operators
  Power     lhs rhs -> binaryR lhs " ^ "  rhs

  Negate lhs@(Literal x)                | numeric x -> negateParen lhs
  Negate lhs@(Literal x `Index` _)      | numeric x -> negateParen lhs
  Negate lhs@(Literal x `Range` _)      | numeric x -> negateParen lhs
  Negate lhs@(Literal Flt{} `PropertyRef` _)        -> negateParen lhs
  Negate lhs@(VerbCall (Literal x) _ _) | numeric x -> negateParen lhs
  Negate lhs -> ("-" <>) <$> parenL expr lhs

  Not lhs -> ("!" <>) <$> parenL expr lhs

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

  where binaryL :: Expr -> Builder -> Expr -> Unparser Builder
        binaryL lhs op rhs = do
          lhs' <- parenL expr lhs
          rhs' <- parenR expr rhs
          return $ lhs' <> op <> rhs'

        binaryR :: Expr -> Builder -> Expr -> Unparser Builder
        binaryR lhs op rhs = do
          lhs' <- parenR expr lhs
          rhs' <- parenL expr rhs
          return $ lhs' <> op <> rhs'

        numeric :: Value -> Bool
        numeric Int{} = True
        numeric Flt{} = True
        numeric _     = False

        negateParen :: Expr -> Unparser Builder
        negateParen = fmap ("-" <>) . paren

unparseArgs :: [Argument] -> Unparser Builder
unparseArgs = fmap (mconcat . intersperse ", ") . mapM unparseArg
  where unparseArg (ArgNormal expr) =              unparseExpr expr
        unparseArg (ArgSplice expr) = ("@" <>) <$> unparseExpr expr

unparseScatter :: [ScatterItem] -> Unparser Builder
unparseScatter = fmap (mconcat . intersperse ", ") . mapM unparseScat
  where unparseScat (ScatRequired var)         = return $        fromId var
        unparseScat (ScatRest     var)         = return $ "@" <> fromId var
        unparseScat (ScatOptional var Nothing) = return $ "?" <> fromId var
        unparseScat (ScatOptional var (Just expr)) = do
          expr' <- unparseExpr expr
          return $ "?" <> fromId var <> " = " <> expr'

unparseNameExpr :: Expr -> Unparser Builder
unparseNameExpr (Literal (Str name))
  | isIdentifier name = return (Str.toBuilder name)
unparseNameExpr expr = paren expr

paren :: Expr -> Unparser Builder
paren expr = do
  expr' <- unparseExpr expr
  return $ "(" <> expr' <> ")"

mightParen :: (Int -> Int -> Bool) -> Expr -> Expr -> Unparser Builder
mightParen cmp parent child = do
  fullyParenthesizing <- asks fullyParenthesizing
  if (fullyParenthesizing && precedence child < precedence PropertyRef{}) ||
     (precedence parent `cmp` precedence child)
    then paren child
    else unparseExpr child

parenL :: Expr -> Expr -> Unparser Builder
parenL = mightParen (>)

parenR :: Expr -> Expr -> Unparser Builder
parenR = mightParen (>=)

isIdentifier :: StrT -> Bool
isIdentifier name = isIdentifier' (Str.toString name) && not (isKeyword name)
  where isIdentifier' :: String -> Bool
        isIdentifier' (c:cs) = isIdentStart c && all isIdentChar cs
        isIdentifier'  []    = False

        isIdentStart, isIdentChar :: Char -> Bool
        isIdentStart c = isAlpha    c || c == '_'
        isIdentChar  c = isAlphaNum c || c == '_'

isKeyword :: StrT -> Bool
isKeyword = (`HS.member` keywordsSet) . toId

keywordsSet :: HashSet Id
keywordsSet = HS.fromList (map toId keywords)
