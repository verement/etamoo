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

import Control.Concurrent.STM (STM, TVar)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Cont (ContT)
import Control.Monad.State.Strict (StateT)

import {-# SOURCE #-} MOO.Command
import {-# SOURCE #-} MOO.Object
import MOO.Types
import {-# SOURCE #-} MOO.Verb

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
notyet :: StrT -> MOO a
