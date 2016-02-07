-- -*- Haskell -*-

module MOO.Compiler (compile) where

import MOO.AST
import {-# SOURCE #-} MOO.Task
import MOO.Types

compile :: Program -> MOO Value
