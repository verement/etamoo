
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
               ) where

import MOO.Types

newtype Program = Program [Statement]
                deriving Show

data Statement = Expression Expr
               | If         Expr Then [ElseIf] Else
               | ForList    Id  Expr        [Statement]
               | ForRange   Id (Expr, Expr) [Statement]
               | While      (Maybe Id) Expr [Statement]
               | Fork       (Maybe Id) Expr [Statement]
               | Break      (Maybe Id)
               | Continue   (Maybe Id)
               | Return     (Maybe Expr)
               | TryExcept  [Statement] [Except]
               | TryFinally [Statement] Finally
               deriving Show

newtype Then    = Then        [Statement]             deriving Show
data    ElseIf  = ElseIf Expr [Statement]             deriving Show
newtype Else    = Else        [Statement]             deriving Show

data    Except  = Except (Maybe Id) Codes [Statement] deriving Show
newtype Finally = Finally                 [Statement] deriving Show

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
