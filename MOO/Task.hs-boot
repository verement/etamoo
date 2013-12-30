-- -*- Haskell -*-

module MOO.Task ( MOO
                , delayIO
                , getObject
                , readProperty
                , findVerb
                , callCommandVerb
                , notyet
                ) where

import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.State
import Control.Concurrent.STM

import MOO.Types
import {-# SOURCE #-} MOO.Object
import {-# SOURCE #-} MOO.Verb
import {-# SOURCE #-} MOO.Command

type MOO = ReaderT Environment
           (ContT TaskDisposition
            (StateT TaskState STM))

data Environment
data TaskDisposition
data TaskState

delayIO :: IO () -> MOO ()

getObject :: ObjId -> MOO (Maybe Object)
readProperty :: ObjId -> StrT -> MOO (Maybe Value)
findVerb :: (Verb -> Bool) -> StrT -> ObjId -> MOO (Maybe ObjId, Maybe Verb)
callCommandVerb :: ObjId -> (ObjId, Verb) -> ObjId ->
                   Command -> (ObjId, ObjId) -> MOO Value

notyet :: String -> MOO a
