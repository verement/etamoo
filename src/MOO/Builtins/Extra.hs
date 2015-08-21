
{-# LANGUAGE CPP #-}

module MOO.Builtins.Extra (builtins) where

import MOO.Builtins.Common

# ifdef MOO_WAIF
import qualified MOO.Builtins.Extra.WAIF as WAIF
# endif

builtins :: [Builtin]
builtins = []
# ifdef MOO_WAIF
  ++ WAIF.builtins
# endif
