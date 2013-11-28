-- -*- Haskell -*-

module MOO.Builtins ( builtinFunctions, callBuiltin ) where

import Data.Map (Map)

import MOO.Types
import MOO.Execution
import MOO.Builtins.Common

builtinFunctions :: Map Id (Builtin, Info)
callBuiltin :: Id -> [Value] -> MOO Value
