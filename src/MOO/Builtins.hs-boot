-- -*- Haskell -*-

module MOO.Builtins ( builtinFunctions, callBuiltin ) where

import Data.Map (Map)

import MOO.Types
import MOO.Task
import MOO.Builtins.Common

builtinFunctions :: Map Id Builtin
callBuiltin :: Id -> [Value] -> MOO Value
