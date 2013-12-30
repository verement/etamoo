
module MOO.Network ( Connection
                   , bootPlayer
                   , notify
                   ) where

import MOO.Types
import {-# SOURCE #-} MOO.Task

import qualified Data.Text as T

data Connection

bootPlayer :: ObjId -> MOO ()
bootPlayer oid = notyet "bootPlayer"

notify :: ObjId -> StrT -> MOO ()
notify who what = delayIO (putStrLn $ T.unpack what)
