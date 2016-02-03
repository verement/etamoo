
{-# LANGUAGE ForeignFunctionInterface #-}

module MOO.Version (version, serverVersion, lmdbVersion, pcreVersion) where

import Data.Text (Text, pack)
import Data.Version (showVersion)
import Foreign (Ptr, nullPtr)
import Foreign.C (CString, CInt, peekCString)
import System.IO.Unsafe (unsafePerformIO)

import Paths_EtaMOO (version)

import MOO.Builtins.Match (pcreVersion)

foreign import ccall unsafe "static lmdb.h"
  mdb_version :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CString

-- | The current version of the server code, as a displayable 'Text' value
-- (for use by the @server_version()@ built-in function)
serverVersion :: Text
serverVersion = pack $ "EtaMOO/" ++ showVersion version

lmdbVersion :: String
{-# NOINLINE lmdbVersion #-}
lmdbVersion = unsafePerformIO
              (peekCString =<< mdb_version nullPtr nullPtr nullPtr)
