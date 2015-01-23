
module MOO.Version ( serverVersion ) where

import Data.Text (Text, pack)
import Data.Version (showVersion)

import Paths_EtaMOO (version)

-- | The current version of the server code, as a displayable 'Text' value
-- (for use by the @server_version()@ built-in function)
serverVersion :: Text
serverVersion = pack $ "EtaMOO/" ++ showVersion version
