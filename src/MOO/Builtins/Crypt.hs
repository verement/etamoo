
{-# LANGUAGE ForeignFunctionInterface #-}

module MOO.Builtins.Crypt (crypt) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Exception (bracket)
import Foreign (nullPtr)
import Foreign.C (CString, withCString, peekCString)
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall safe "static unistd.h crypt"
  c_crypt :: CString -> CString -> IO CString

-- | Encrypt a password using the POSIX @crypt()@ function.
crypt :: String             -- ^ key
      -> String             -- ^ salt
      -> IO (Maybe String)  -- ^ encrypted password (or 'Nothing' on error)
crypt key salt =
  withCString key  $ \c_key  ->
  withCString salt $ \c_salt ->
  bracket (takeMVar mutex) (putMVar mutex) $ \_ -> do
    result <- c_crypt c_key c_salt
    if result == nullPtr
      then return Nothing
      else Just <$> peekCString result

-- Shared mutex to prevent simultaneous calls to non-reentrant C crypt()
mutex :: MVar ()
{-# NOINLINE mutex #-}
mutex = unsafePerformIO $ newMVar ()
