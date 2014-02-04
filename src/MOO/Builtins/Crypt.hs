
{-# LANGUAGE ForeignFunctionInterface #-}
{-# CFILES src/cbits/crypt.c #-}

module MOO.Builtins.Crypt (crypt) where

import Control.Monad (liftM)
import Foreign (allocaBytes)
import Foreign.C (CString, CInt(..), withCString, peekCString)
import System.IO.Unsafe (unsafePerformIO)

-- This must be /unsafe/ to block other threads while it executes, since it
-- relies on a non-reentrant C function.
foreign import ccall unsafe "static crypt.h"
  crypt_helper :: CString -> CString -> CString -> CInt -> IO CInt

-- | Encrypt a password using the POSIX @crypt()@ function.
crypt :: String        -- ^ key
      -> String        -- ^ salt
      -> Maybe String  -- ^ encrypted password (or 'Nothing' on error)
crypt key salt =
  unsafePerformIO  $
  withCString key  $ \c_key  ->
  withCString salt $ \c_salt -> crypt' c_key c_salt 16

crypt' :: CString -> CString -> CInt -> IO (Maybe String)
crypt' key salt len =
  allocaBytes (fromIntegral len) $ \encrypted -> do
    result <- crypt_helper key salt encrypted len
    case result of
      0   -> Just `liftM` peekCString encrypted
      -1  -> return Nothing
      req -> crypt' key salt req
