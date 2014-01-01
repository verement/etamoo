
module MOO.Version ( serverVersion
                   , serverVersionText
                   ) where

import Data.Text
import Data.Version

serverVersion :: Version
serverVersion = Version {
    versionBranch = [2,0,0]
  , versionTags   = ["alpha"]
  }

serverVersionText :: Text
serverVersionText = pack (showVersion serverVersion)
