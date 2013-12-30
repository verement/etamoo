
{-# LANGUAGE OverloadedStrings #-}

module MOO.Unparser ( unparse ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Char
import Data.Set (Set)
import Data.Text (Text)

import qualified Data.Set as S
import qualified Data.Text as T

import MOO.AST
import MOO.Types
import MOO.Parser (keywords)

type Unparser = ReaderT UnparserEnv (Writer Text)

data UnparserEnv = UnparserEnv {
    fullyParenthesizing :: Bool
  , indenting           :: Bool
  , indentation         :: StrT
}

initUnparserEnv = UnparserEnv {
    fullyParenthesizing = False
  , indenting           = False
  , indentation         = ""
}

unparse :: Bool -> Bool -> Program -> Text
unparse fullyParen indent (Program stmts) =
  execWriter $ runReaderT (tellStatements stmts) $ initUnparserEnv {
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
            indent >> tell "except"
            maybeTellVar var
            tell " ("
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
  indent >> tell name >> maybeTellVar maybeVar >> detail
  moreIndented $ tellStatements body
  indent >> tell "end" >> tell name >> tell "\n"

maybeTellVar :: Maybe Id -> Unparser ()
maybeTellVar Nothing    = return ()
maybeTellVar (Just var) = tell " " >> tell var

detailExpr :: Expr -> Unparser ()
detailExpr expr = tell " (" >> tellExpr expr >> tell ")\n"

tellExpr :: Expr -> Unparser ()
tellExpr = tell <=< unparseExpr

unparseExpr :: Expr -> Unparser Text
unparseExpr expr = case expr of
  Literal value -> return $ toLiteral value

  List args -> do
    args' <- unparseArgs args
    return $ "{" <> args' <> "}"

  Variable var -> return var

  PropRef (Literal (Obj 0)) (Literal (Str name))
    | isIdentifier name -> return $ "$" <> name
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
                              return $ "$" <> name <> "(" <> args' <> ")"
  VerbCall obj name args -> do
    obj' <- parenL expr obj
    name' <- unparseNameExpr name
    args' <- unparseArgs args
    return $ obj' <> ":" <> name' <> "(" <> args' <> ")"

  BuiltinFunc func args -> do
    args' <- unparseArgs args
    return $ func <> "(" <> args' <> ")"

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

unparseArgs :: [Arg] -> Unparser Text
unparseArgs = liftM (T.intercalate ", ") . mapM unparseArg
  where unparseArg (ArgNormal expr) = unparseExpr expr
        unparseArg (ArgSplice expr) = ("@" <>) `liftM` unparseExpr expr

unparseScatter :: [ScatItem] -> Unparser Text
unparseScatter = liftM (T.intercalate ", ") . mapM unparseScat
  where unparseScat (ScatRequired var)             = return var
        unparseScat (ScatRest     var)             = return $ "@" <> var
        unparseScat (ScatOptional var Nothing)     = return $ "?" <> var
        unparseScat (ScatOptional var (Just expr)) = do
          expr' <- unparseExpr expr
          return $ "?" <> var <> " = " <> expr'

unparseNameExpr :: Expr -> Unparser Text
unparseNameExpr (Literal (Str name))
  | isIdentifier name = return name
unparseNameExpr expr = do
  expr' <- unparseExpr expr
  return $ "(" <> expr' <> ")"

paren :: Expr -> Unparser Text
paren expr = do
  expr' <- unparseExpr expr
  return $ "(" <> expr' <> ")"

mightParen :: (Int -> Int -> Bool) -> Expr -> Expr -> Unparser Text
mightParen cmp parent child = do
  fullyParenthesizing <- asks fullyParenthesizing
  if (fullyParenthesizing && precedence child < precedence PropRef{}) ||
     (precedence parent `cmp` precedence child)
    then paren child
    else unparseExpr child

parenL :: Expr -> Expr -> Unparser Text
parenL = mightParen (>)

parenR :: Expr -> Expr -> Unparser Text
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
