-- -*- Haskell -*-

module MOO.Task ( MOO ) where

import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.State
import Control.Concurrent.STM

type MOO = ReaderT Environment
           (ContT TaskDisposition
            (StateT TaskState STM))

data Environment
data TaskDisposition
data TaskState
