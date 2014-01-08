
module MOO.Builtins.Common ( BuiltinSpec, Builtin, Info(..)
                           , defaults, booleanDefaults, maybeDefaults ) where

import MOO.Types
import MOO.Task

-- | A pair associating a named built-in function with its implementation and
-- a description of the required arguments
type BuiltinSpec = (Id, (Builtin, Info))

-- | The type of each built-in function implementation
type Builtin = [Value] -> MOO Value

-- | A description of the number and types of required and optional arguments
--
-- @Info@ /min/ /max/ @[@/types/@]@ /return-type/ describes a function
-- requiring at least /min/ and at most /max/ arguments (or no maximum if
-- /max/ is 'Nothing'), with each argument having the specified 'Type'. The
-- /return-type/ is for documentation purposes and is not otherwise currently
-- used.
data Info = Info Int (Maybe Int) [Type] Type

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
