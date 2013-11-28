
module MOO.Builtins.Common ( BuiltinSpec, Builtin, Info(..), boolean ) where

import MOO.Types
import MOO.Execution

type BuiltinSpec = (Id, (Builtin, Info))

type Builtin = [Value] -> MOO Value

data Info = Info Int (Maybe Int) [Type] Type

boolean :: [Value] -> Bool
boolean (v:_) = truthOf v
boolean []    = False
