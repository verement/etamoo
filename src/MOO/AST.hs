
{-# LANGUAGE DeriveDataTypeable #-}

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
  , Argument(..)
  , ScatterItem(..)

  -- * Utility Functions
  , isLValue
  , precedence

  ) where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Database.VCache (VCacheable(put, get), VPut, VGet, putWord8)

import MOO.Types

newtype Program = Program [Statement]
                deriving (Show, Typeable)

instance VCacheable Program where
  put (Program smts) = put smts
  get = Program <$> get

instance Sizeable Program where
  storageBytes (Program stmts) = storageBytes stmts

data Statement = Expression !LineNo Expr
               | If         !LineNo Expr Then [ElseIf] Else
               | ForList    !LineNo Id  Expr        [Statement]
               | ForRange   !LineNo Id (Expr, Expr) [Statement]
               | While      !LineNo (Maybe Id) Expr [Statement]
               | Fork       !LineNo (Maybe Id) Expr [Statement]
               | Break              (Maybe Id)
               | Continue           (Maybe Id)
               | Return     !LineNo (Maybe Expr)
               | TryExcept          [Statement] [Except]
               | TryFinally         [Statement] Finally
               deriving (Show, Typeable)

newtype Then    = Then                [Statement]      deriving (Show, Typeable)

data    ElseIf  = ElseIf !LineNo Expr [Statement]      deriving (Show, Typeable)
newtype Else    = Else                [Statement]      deriving (Show, Typeable)

data    Except  = Except !LineNo (Maybe Id) Codes [Statement]
                deriving (Show, Typeable)
newtype Finally = Finally                         [Statement]
                deriving (Show, Typeable)

instance VCacheable Statement where
  put s = case s of
    Expression line expr             -> putTag2 00 line expr
    If line expr thens elseIfs elses -> putTag5 01 line expr thens elseIfs elses
    ForList  line var expr  smts     -> putTag4 02 line var  expr  smts
    ForRange line var range smts     -> putTag4 03 line var  range smts
    While    line var expr  smts     -> putTag4 04 line var  expr  smts
    Fork     line var expr  smts     -> putTag4 05 line var  expr  smts
    Break    var                     -> putTag1 06 var
    Continue var                     -> putTag1 07 var
    Return line expr                 -> putTag2 08 line expr
    TryExcept  smts excepts          -> putTag2 09 smts excepts
    TryFinally smts finally          -> putTag2 10 smts finally

  get = get >>= \tag -> case tag of
    00 -> Expression <$> get <*> get
    01 -> If         <$> get <*> get <*> get <*> get <*> get
    02 -> ForList    <$> get <*> get <*> get <*> get
    03 -> ForRange   <$> get <*> get <*> get <*> get
    04 -> While      <$> get <*> get <*> get <*> get
    05 -> Fork       <$> get <*> get <*> get <*> get
    06 -> Break      <$> get
    07 -> Continue   <$> get
    08 -> Return     <$> get <*> get
    09 -> TryExcept  <$> get <*> get
    10 -> TryFinally <$> get <*> get
    _  -> unknownTag "Statement" tag

instance VCacheable Then where
  put (Then smts) = put smts
  get = Then <$> get

instance VCacheable ElseIf where
  put (ElseIf line expr smts) = put line >> put expr >> put smts
  get = ElseIf <$> get <*> get <*> get

instance VCacheable Else where
  put (Else smts) = put smts
  get = Else <$> get

instance VCacheable Except where
  put (Except line var codes smts) =
    put line >> put var >> put codes >> put smts
  get = Except <$> get <*> get <*> get <*> get

instance VCacheable Finally where
  put (Finally smts) = put smts
  get = Finally <$> get

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
          | List [Argument]

          | Variable Id
          | PropertyRef Expr Expr

          | Assign Expr Expr
          | Scatter [ScatterItem] Expr

          | VerbCall Expr Expr [Argument]
          | BuiltinFunc Id [Argument]

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

          | Expr `CompareEQ` Expr
          | Expr `CompareNE` Expr
          | Expr `CompareLT` Expr
          | Expr `CompareLE` Expr
          | Expr `CompareGT` Expr
          | Expr `CompareGE` Expr

          | Catch Expr Codes Default

          deriving (Show, Typeable)

instance VCacheable Expr where
  put expr = case expr of
    Literal val            -> putTag1 00 val
    List args              -> putTag1 01 args
    Variable var           -> putTag1 02 var
    PropertyRef obj name   -> putTag2 03 obj   name
    Assign var expr        -> putTag2 04 var   expr
    Scatter items expr     -> putTag2 05 items expr
    VerbCall obj name args -> putTag3 06 obj   name  args
    BuiltinFunc name args  -> putTag2 07 name  args
    expr `Index` index     -> putTag2 08 expr  index
    expr `Range` range     -> putTag2 09 expr  range
    Length                 -> putTag0 10
    item `In` expr         -> putTag2 11 item  expr
    x `Plus`   y           -> putTag2 12 x     y
    x `Minus`  y           -> putTag2 13 x     y
    x `Times`  y           -> putTag2 14 x     y
    x `Divide` y           -> putTag2 15 x     y
    x `Remain` y           -> putTag2 16 x     y
    x `Power`  y           -> putTag2 17 x     y
    Negate expr            -> putTag1 18 expr
    Conditional cond x y   -> putTag3 19 cond  x     y
    x `And` y              -> putTag2 20 x     y
    x `Or`  y              -> putTag2 21 x     y
    Not expr               -> putTag1 22 expr
    x `CompareEQ` y        -> putTag2 23 x     y
    x `CompareNE` y        -> putTag2 24 x     y
    x `CompareLT` y        -> putTag2 25 x     y
    x `CompareLE` y        -> putTag2 26 x     y
    x `CompareGT` y        -> putTag2 27 x     y
    x `CompareGE` y        -> putTag2 28 x     y
    Catch expr codes def   -> putTag3 29 expr  codes def

  get = get >>= \tag -> case tag of
    00 -> Literal     <$> get
    01 -> List        <$> get
    02 -> Variable    <$> get
    03 -> PropertyRef <$> get <*> get
    04 -> Assign      <$> get <*> get
    05 -> Scatter     <$> get <*> get
    06 -> VerbCall    <$> get <*> get <*> get
    07 -> BuiltinFunc <$> get <*> get
    08 -> Index       <$> get <*> get
    09 -> Range       <$> get <*> get
    10 -> pure Length
    11 -> In          <$> get <*> get
    12 -> Plus        <$> get <*> get
    13 -> Minus       <$> get <*> get
    14 -> Times       <$> get <*> get
    15 -> Divide      <$> get <*> get
    16 -> Remain      <$> get <*> get
    17 -> Power       <$> get <*> get
    18 -> Negate      <$> get
    19 -> Conditional <$> get <*> get <*> get
    20 -> And         <$> get <*> get
    21 -> Or          <$> get <*> get
    22 -> Not         <$> get
    23 -> CompareEQ   <$> get <*> get
    24 -> CompareNE   <$> get <*> get
    25 -> CompareLT   <$> get <*> get
    26 -> CompareLE   <$> get <*> get
    27 -> CompareGT   <$> get <*> get
    28 -> CompareGE   <$> get <*> get
    29 -> Catch       <$> get <*> get <*> get
    _  -> unknownTag "Expr" tag

instance Sizeable Expr where
  storageBytes (Literal value) = storageBytes () + storageBytes value
  storageBytes (List args)     = storageBytes () + storageBytes args
  storageBytes (Variable var)  = storageBytes () + storageBytes var
  storageBytes (PropertyRef obj name) =
    storageBytes () + storageBytes obj + storageBytes name
  storageBytes (Assign lhs rhs) =
    storageBytes () + storageBytes lhs + storageBytes rhs
  storageBytes (Scatter scats expr) =
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
  storageBytes (CompareEQ x y) =
    storageBytes () + storageBytes x + storageBytes y
  storageBytes (CompareNE x y) =
    storageBytes () + storageBytes x + storageBytes y
  storageBytes (CompareLT x y) =
    storageBytes () + storageBytes x + storageBytes y
  storageBytes (CompareLE x y) =
    storageBytes () + storageBytes x + storageBytes y
  storageBytes (CompareGT x y) =
    storageBytes () + storageBytes x + storageBytes y
  storageBytes (CompareGE x y) =
    storageBytes () + storageBytes x + storageBytes y
  storageBytes (Catch expr codes (Default dv)) =
    storageBytes () + storageBytes expr + storageBytes codes + storageBytes dv

data    Codes   = ANY | Codes [Argument] deriving (Show, Typeable)
newtype Default = Default (Maybe Expr)   deriving (Show, Typeable)

instance VCacheable Codes where
  put codes = put $ case codes of
    ANY         -> Nothing
    Codes codes -> Just codes
  get = maybe ANY Codes <$> get

instance VCacheable Default where
  put (Default expr) = put expr
  get = Default <$> get

instance Sizeable Codes where
  storageBytes ANY          = storageBytes ()
  storageBytes (Codes args) = storageBytes () + storageBytes args

data Argument = ArgNormal Expr
              | ArgSplice Expr
              deriving (Show, Typeable)

instance VCacheable Argument where
  put arg = put $ case arg of
    ArgNormal expr -> Left  expr
    ArgSplice expr -> Right expr
  get = either ArgNormal ArgSplice <$> get

instance Sizeable Argument where
  storageBytes (ArgNormal expr) = storageBytes () + storageBytes expr
  storageBytes (ArgSplice expr) = storageBytes () + storageBytes expr

data ScatterItem = ScatRequired Id
                 | ScatOptional Id (Maybe Expr)
                 | ScatRest     Id
                 deriving (Show, Typeable)

instance VCacheable ScatterItem where
  put item = case item of
    ScatRequired var      -> putTag1 0 var
    ScatOptional var expr -> putTag2 1 var expr
    ScatRest     var      -> putTag1 2 var

  get = get >>= \tag -> case tag of
    0 -> ScatRequired <$> get
    1 -> ScatOptional <$> get <*> get
    2 -> ScatRest     <$> get
    _ -> unknownTag "ScatterItem" tag

instance Sizeable ScatterItem where
  storageBytes (ScatRequired var) = storageBytes () + storageBytes var
  storageBytes (ScatOptional var expr) =
    storageBytes () + storageBytes var + storageBytes expr
  storageBytes (ScatRest     var) = storageBytes () + storageBytes var

-- | Can the given expression be used on the left-hand side of an assignment?
isLValue :: Expr -> Bool
isLValue (Range e _)  = isLValue' e
isLValue e            = isLValue' e

isLValue' :: Expr -> Bool
isLValue' Variable{}    = True
isLValue' PropertyRef{} = True
isLValue' (Index e _)   = isLValue' e
isLValue' _             = False

-- | Return a precedence value for the given expression, needed by
-- "MOO.Unparser" to determine whether parentheses are necessary to isolate an
-- expression from its surrounding context.
precedence :: Expr -> Int
precedence expr = case expr of
  Assign{}      ->  1
  Scatter{}     ->  1

  Conditional{} ->  2

  And{}         ->  3
  Or{}          ->  3

  CompareEQ{}   ->  4
  CompareNE{}   ->  4
  CompareLT{}   ->  4
  CompareLE{}   ->  4
  CompareGT{}   ->  4
  CompareGE{}   ->  4
  In{}          ->  4

  Plus{}        ->  5
  Minus{}       ->  5

  Times{}       ->  6
  Divide{}      ->  6
  Remain{}      ->  6

  Power{}       ->  7

  Not{}         ->  8
  Negate{}      ->  8

  PropertyRef{} ->  9
  VerbCall{}    ->  9
  Index{}       ->  9
  Range{}       ->  9

  _             -> 10

putTag0 :: Word8 -> VPut ()
putTag0 = putWord8

putTag1 :: VCacheable a => Word8 -> a -> VPut ()
putTag1 tag a = putWord8 tag >> put a

putTag2 :: (VCacheable a, VCacheable b) => Word8 -> a -> b -> VPut ()
putTag2 tag a b = putWord8 tag >> put a >> put b

putTag3 :: (VCacheable a, VCacheable b, VCacheable c) =>
           Word8 -> a -> b -> c -> VPut ()
putTag3 tag a b c = putWord8 tag >> put a >> put b >> put c

putTag4 :: (VCacheable a, VCacheable b, VCacheable c, VCacheable d) =>
           Word8 -> a -> b -> c -> d -> VPut ()
putTag4 tag a b c d = putWord8 tag >> put a >> put b >> put c >> put d

putTag5 :: (VCacheable a, VCacheable b, VCacheable c, VCacheable d,
            VCacheable e) => Word8 -> a -> b -> c -> d -> e -> VPut ()
putTag5 tag a b c d e =
  putWord8 tag >> put a >> put b >> put c >> put d >> put e

unknownTag :: String -> Word8 -> VGet a
unknownTag what tag =
  fail $ "get: unknown " ++ what ++ " tag (" ++ show tag ++ ")"
