-- -*- Haskell -*-

module MOO.Task ( MOO
                , World
                , requestIO
                , delayIO
                , getWorld
                , getWorld'
                , putWorld
                , getPlayer
                , getObject
                , readProperty
                , findVerb
                , callCommandVerb
                , raise
                , notyet
                ) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.State.Strict

import MOO.Types
import {-# SOURCE #-} MOO.Object
import {-# SOURCE #-} MOO.Verb
import {-# SOURCE #-} MOO.Command

data World

data Environment
data TaskDisposition
data TaskState

type MOO = ReaderT Environment
           (ContT TaskDisposition
            (StateT TaskState STM))

requestIO :: IO a -> MOO a
delayIO :: IO () -> MOO ()

getWorld :: MOO World
getWorld' :: MOO (TVar World)
putWorld :: World -> MOO ()
getPlayer :: MOO ObjId
getObject :: ObjId -> MOO (Maybe Object)
readProperty :: ObjId -> StrT -> MOO (Maybe Value)
findVerb :: (Verb -> Bool) -> StrT -> ObjId -> MOO (Maybe ObjId, Maybe Verb)
callCommandVerb :: ObjId -> (ObjId, Verb) -> ObjId ->
                   Command -> (ObjId, ObjId) -> MOO Value

raise :: Error -> MOO a
notyet :: String -> MOO a
