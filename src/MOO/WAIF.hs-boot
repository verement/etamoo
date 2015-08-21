-- -*- Haskell -*-

module MOO.WAIF (WAIF, mangleWaifVerbName) where

import Database.VCache (VCacheable)

import MOO.String

data WAIF

instance Eq WAIF
instance Show WAIF
instance VCacheable WAIF

mangleWaifVerbName :: MOOString -> MOOString
