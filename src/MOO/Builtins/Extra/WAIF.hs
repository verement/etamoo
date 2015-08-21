
{-# LANGUAGE OverloadedStrings #-}

module MOO.Builtins.Extra.WAIF (builtins) where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))

import MOO.Builtins.Common
import MOO.Task
import MOO.Types
import MOO.WAIF

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

builtins :: [Builtin]
builtins = [bf_new_waif]

bf_new_waif = Builtin "new_waif" 0 (Just 0) [] TWaf $ \[] -> do
  (this, waifOwner) <- frame (initialThis &&& permissions)
  let waifClass' = case this of
        Obj x -> x
        Waf x -> waifClass x
  liftVTx $ Waf <$> newWaif waifClass' waifOwner
