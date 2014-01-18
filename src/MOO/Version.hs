
module MOO.Version ( serverVersion
                   , serverVersionText
                   ) where

import Data.Text (Text, pack)
import Data.Version (Version, showVersion)

import Paths_EtaMOO (version)

-- | The current version of the server code, as a 'Version' value
serverVersion :: Version
serverVersion = version

-- | The current version of the server code, as a displayable 'Text' value
-- (for use by the @server_version()@ built-in function)
serverVersionText :: Text
serverVersionText = pack (showVersion serverVersion)
