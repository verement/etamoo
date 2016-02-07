-- -*- Haskell -*-

module MOO.Builtins (builtinFunctions, callBuiltin) where

import Data.HashMap.Lazy (HashMap)

import MOO.Types
import MOO.Task
import MOO.Builtins.Common

builtinFunctions :: HashMap Id Builtin
callBuiltin :: Id -> [Value] -> MOO Value
