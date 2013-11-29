
module MOO.Builtins.Common ( BuiltinSpec, Builtin, Info(..)
                           , defaults, booleanDefaults, maybeDefaults ) where

import MOO.Types
import MOO.Execution

type BuiltinSpec = (Id, (Builtin, Info))

type Builtin = [Value] -> MOO Value

data Info = Info Int (Maybe Int) [Type] Type

defaultOptions :: (Value -> a) -> [Value] -> [a] -> [a]
defaultOptions f = defaultOptions'
  where defaultOptions' (v:vs) (_:ds) = f v : defaultOptions' vs ds
        defaultOptions' []     (d:ds) = d   : defaultOptions' [] ds
        defaultOptions' []     []     = []
        defaultOptions' (v:vs) []     = error "excess options"

defaults :: [Value] -> [Value] -> [Value]
defaults = defaultOptions id

booleanDefaults :: [Value] -> [Bool] -> [Bool]
booleanDefaults = defaultOptions truthOf

maybeDefaults :: [Value] -> [Maybe Value]
maybeDefaults = flip (defaultOptions Just) (repeat Nothing)
