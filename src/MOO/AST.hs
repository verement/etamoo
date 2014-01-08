
-- | Data structures representing an abstract syntax tree for MOO code
module MOO.AST (

  -- * Data Structures
    Program(..)
  , Statement(..)
  , Then(..)
  , ElseIf(..)
  , Else(..)
  , Except(..)
  , Finally(..)
  , Expr(..)
  , Codes(..)
  , Default(..)
  , Arg(..)
  , ScatItem(..)

  -- * Utility Functions
  , isLValue
  , precedence

  ) where

import MOO.Types

newtype Program = Program [Statement]
                deriving Show

instance Sizeable Program where
  storageBytes (Program stmts) = storageBytes stmts

data Statement = Expression !Int Expr
               | If         !Int Expr Then [ElseIf] Else
               | ForList    !Int Id  Expr        [Statement]
               | ForRange   !Int Id (Expr, Expr) [Statement]
               | While      !Int (Maybe Id) Expr [Statement]
               | Fork       !Int (Maybe Id) Expr [Statement]
               | Break           (Maybe Id)
               | Continue        (Maybe Id)
               | Return     !Int (Maybe Expr)
               | TryExcept       [Statement] [Except]
               | TryFinally      [Statement] Finally
               deriving Show

newtype Then    = Then             [Statement]             deriving Show
data    ElseIf  = ElseIf !Int Expr [Statement]             deriving Show
newtype Else    = Else             [Statement]             deriving Show

data    Except  = Except !Int (Maybe Id) Codes [Statement] deriving Show
newtype Finally = Finally                      [Statement] deriving Show

instance Sizeable Statement where
  storageBytes (Expression line expr) =
    storageBytes () + storageBytes line + storageBytes expr
  storageBytes (If line expr (Then thens) elseIfs (Else elses)) =
    storageBytes () + storageBytes line + storageBytes expr +
    storageBytes thens + storageBytes elseIfs + storageBytes elses
  storageBytes (ForList line var expr body) =
    storageBytes () + storageBytes line + storageBytes var +
    storageBytes expr + storageBytes body
  storageBytes (ForRange line var range body) =
    storageBytes () + storageBytes line + storageBytes var +
    storageBytes range + storageBytes body
  storageBytes (While line var expr body) =
    storageBytes () + storageBytes line + storageBytes var +
    storageBytes expr + storageBytes body
  storageBytes (Fork line var expr body) =
    storageBytes () + storageBytes line + storageBytes var +
    storageBytes expr + storageBytes body
  storageBytes (Break    var) = storageBytes () + storageBytes var
  storageBytes (Continue var) = storageBytes () + storageBytes var
  storageBytes (Return line expr) =
    storageBytes () + storageBytes line + storageBytes expr
  storageBytes (TryExcept body excepts) =
    storageBytes () + storageBytes body + storageBytes excepts
  storageBytes (TryFinally body (Finally finally)) =
    storageBytes () + storageBytes body + storageBytes finally

instance Sizeable ElseIf where
  storageBytes (ElseIf line expr body) =
    storageBytes () + storageBytes line + storageBytes expr + storageBytes body

instance Sizeable Except where
  storageBytes (Except line var codes body) =
    storageBytes () + storageBytes line + storageBytes var +
    storageBytes codes + storageBytes body

data Expr = Literal Value
          | List [Arg]

          | Variable Id
          | PropRef Expr Expr

          | Assign Expr Expr
          | ScatterAssign [ScatItem] Expr

          | VerbCall Expr Expr [Arg]
          | BuiltinFunc Id [Arg]

          | Expr `Index`  Expr
          | Expr `Range` (Expr, Expr)
          | Length

          | Expr `In` Expr

          | Expr `Plus`   Expr
          | Expr `Minus`  Expr
          | Expr `Times`  Expr
          | Expr `Divide` Expr
          | Expr `Remain` Expr
          | Expr `Power`  Expr
          | Negate Expr

          | Conditional Expr Expr Expr
          | Expr `And` Expr
          | Expr `Or`  Expr
          | Not Expr

          | Expr `Equal`        Expr
          | Expr `NotEqual`     Expr
          | Expr `LessThan`     Expr
          | Expr `LessEqual`    Expr
          | Expr `GreaterThan`  Expr
          | Expr `GreaterEqual` Expr

          | Catch Expr Codes Default

          deriving Show

instance Sizeable Expr where
  storageBytes (Literal value) = storageBytes () + storageBytes value
  storageBytes (List args)     = storageBytes () + storageBytes args
  storageBytes (Variable var)  = storageBytes () + storageBytes var
  storageBytes (PropRef obj name) =
    storageBytes () + storageBytes obj + storageBytes name
  storageBytes (Assign lhs rhs) =
    storageBytes () + storageBytes lhs + storageBytes rhs
  storageBytes (ScatterAssign scats expr) =
    storageBytes () + storageBytes scats + storageBytes expr
  storageBytes (VerbCall obj name args) =
    storageBytes () + storageBytes obj + storageBytes name + storageBytes args
  storageBytes (BuiltinFunc name args) =
    storageBytes () + storageBytes name + storageBytes args
  storageBytes (Index  x y) = storageBytes () + storageBytes x + storageBytes y
  storageBytes (Range  x y) = storageBytes () + storageBytes x + storageBytes y
  storageBytes  Length      = storageBytes ()
  storageBytes (In     x y) = storageBytes () + storageBytes x + storageBytes y
  storageBytes (Plus   x y) = storageBytes () + storageBytes x + storageBytes y
  storageBytes (Minus  x y) = storageBytes () + storageBytes x + storageBytes y
  storageBytes (Times  x y) = storageBytes () + storageBytes x + storageBytes y
  storageBytes (Divide x y) = storageBytes () + storageBytes x + storageBytes y
  storageBytes (Remain x y) = storageBytes () + storageBytes x + storageBytes y
  storageBytes (Power  x y) = storageBytes () + storageBytes x + storageBytes y
  storageBytes (Negate x)   = storageBytes () + storageBytes x
  storageBytes (Conditional x y z) =
    storageBytes () + storageBytes x + storageBytes y + storageBytes z
  storageBytes (And    x y) = storageBytes () + storageBytes x + storageBytes y
  storageBytes (Or     x y) = storageBytes () + storageBytes x + storageBytes y
  storageBytes (Not    x)   = storageBytes () + storageBytes x
  storageBytes (Equal  x y) = storageBytes () + storageBytes x + storageBytes y
  storageBytes (NotEqual x y) =
    storageBytes () + storageBytes x + storageBytes y
  storageBytes (LessThan x y) =
    storageBytes () + storageBytes x + storageBytes y
  storageBytes (LessEqual x y) =
    storageBytes () + storageBytes x + storageBytes y
  storageBytes (GreaterThan x y) =
    storageBytes () + storageBytes x + storageBytes y
  storageBytes (GreaterEqual x y) =
    storageBytes () + storageBytes x + storageBytes y
  storageBytes (Catch expr codes (Default dv)) =
    storageBytes () + storageBytes expr + storageBytes codes + storageBytes dv

data    Codes   = ANY | Codes [Arg]    deriving Show
newtype Default = Default (Maybe Expr) deriving Show

instance Sizeable Codes where
  storageBytes ANY          = storageBytes ()
  storageBytes (Codes args) = storageBytes () + storageBytes args

data Arg = ArgNormal Expr
         | ArgSplice Expr
         deriving Show

instance Sizeable Arg where
  storageBytes (ArgNormal expr) = storageBytes () + storageBytes expr
  storageBytes (ArgSplice expr) = storageBytes () + storageBytes expr

data ScatItem = ScatRequired Id
              | ScatOptional Id (Maybe Expr)
              | ScatRest     Id
              deriving Show

instance Sizeable ScatItem where
  storageBytes (ScatRequired var) = storageBytes () + storageBytes var
  storageBytes (ScatOptional var expr) =
    storageBytes () + storageBytes var + storageBytes expr
  storageBytes (ScatRest     var) = storageBytes () + storageBytes var

-- | Can the given expression be used on the left-hand side of an assignment?
isLValue :: Expr -> Bool
isLValue (Range e _)  = isLValue' e
isLValue e            = isLValue' e

isLValue' :: Expr -> Bool
isLValue' Variable{}  = True
isLValue' PropRef{}   = True
isLValue' (Index e _) = isLValue' e
isLValue' _           = False

-- | Return a precedence value for the given expression, needed by
-- "MOO.Unparser" to determine whether parentheses are necessary to isolate an
-- expression from its surrounding context.
precedence :: Expr -> Int
precedence expr = case expr of
  Assign{}        ->  1
  ScatterAssign{} ->  1

  Conditional{}   ->  2

  And{}           ->  3
  Or{}            ->  3

  Equal{}         ->  4
  NotEqual{}      ->  4
  LessThan{}      ->  4
  LessEqual{}     ->  4
  GreaterThan{}   ->  4
  GreaterEqual{}  ->  4
  In{}            ->  4

  Plus{}          ->  5
  Minus{}         ->  5

  Times{}         ->  6
  Divide{}        ->  6
  Remain{}        ->  6

  Power{}         ->  7

  Not{}           ->  8
  Negate{}        ->  8

  PropRef{}       ->  9
  VerbCall{}      ->  9
  Index{}         ->  9
  Range{}         ->  9

  _               -> 10
