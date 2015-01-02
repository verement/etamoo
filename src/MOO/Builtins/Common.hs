
module MOO.Builtins.Common ( Builtin(..)
                           , defaults, booleanDefaults, maybeDefaults ) where

import MOO.Types
import MOO.Task

-- | A complete description of a builtin function, including its name, the
-- number and types of required and optional arguments, the return type, and
-- the implementing Haskell function
data Builtin = Builtin {
    builtinName       :: Id
  , builtinMinArgs    :: Int
  , builtinMaxArgs    :: Maybe Int
  , builtinArgTypes   :: [Type]
  , builtinReturnType :: Type
  , builtinFunction   :: [Value] -> MOO Value
  }

defaultOptions :: (Value -> a) -> [Value] -> [a] -> [a]
defaultOptions f = defaultOptions'
  where defaultOptions' (v:vs) (_:ds) = f v : defaultOptions' vs ds
        defaultOptions' []     (d:ds) = d   : defaultOptions' [] ds
        defaultOptions' []     []     = []
        defaultOptions' (_:_)  []     = error "excess options"

-- | @defaults@ /optional/ @[@/default-values/@]@ generates an argument list
-- by supplying /default-values/ for any missing from /optional/.
defaults :: [Value] -> [Value] -> [Value]
defaults = defaultOptions id

-- | Generate a list of boolean arguments with the given defaults.
booleanDefaults :: [Value] -> [Bool] -> [Bool]
booleanDefaults = defaultOptions truthOf

-- | Generate an infinite list of arguments where each missing argument is
-- 'Nothing'.
maybeDefaults :: [Value] -> [Maybe Value]
maybeDefaults = flip (defaultOptions Just) (repeat Nothing)
