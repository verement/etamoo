
module MOO.Version (serverVersion, pcreVersion) where

import Data.Text (Text, pack)
import Data.Version (showVersion)

import Paths_EtaMOO (version)

import MOO.Builtins.Match (pcreVersion)

-- | The current version of the server code, as a displayable 'Text' value
-- (for use by the @server_version()@ built-in function)
serverVersion :: Text
serverVersion = pack $ "EtaMOO/" ++ showVersion version
