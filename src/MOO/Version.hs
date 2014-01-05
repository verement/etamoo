
module MOO.Version ( serverVersion
                   , serverVersionText
                   ) where

import Data.Text
import Data.Version

import Paths_EtaMOO

serverVersion :: Version
serverVersion = version

serverVersionText :: Text
serverVersionText = pack (showVersion serverVersion)
