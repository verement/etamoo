-- -*- Haskell -*-

module MOO.List ( MOOList
                , empty, equal, null, toList, fromList ) where

import Database.VCache (VCacheable)

import {-# SOURCE #-} MOO.Types (Value)

import Prelude hiding (null)

data MOOList

instance Eq MOOList
instance Show MOOList
instance VCacheable MOOList

empty :: MOOList
equal :: MOOList -> MOOList -> Bool
null :: MOOList -> Bool
toList :: MOOList -> [Value]
fromList :: [Value] -> MOOList
