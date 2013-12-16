
module MOO.AST ( Program(..)
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
               , isLValue
               , precedence
               ) where

import MOO.Types

newtype Program = Program [Statement]
                deriving Show

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

data    Codes   = ANY | Codes [Arg]    deriving Show
newtype Default = Default (Maybe Expr) deriving Show

data Arg = ArgNormal Expr
         | ArgSplice Expr
         deriving Show

data ScatItem = ScatRequired Id
              | ScatOptional Id (Maybe Expr)
              | ScatRest     Id
              deriving Show

isLValue :: Expr -> Bool
isLValue (Range e _)  = isLValue' e
isLValue e            = isLValue' e

isLValue' Variable{}  = True
isLValue' PropRef{}   = True
isLValue' (Index e _) = isLValue' e
isLValue' _           = False

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
