
module MOO.Database ( ) where

import Data.Vector
import MOO.Object
import MOO.Task

data Database = Database {
    objects     :: Vector Object
  , queuedTasks :: [Task]
  }
