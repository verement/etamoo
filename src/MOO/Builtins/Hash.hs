
{-# LANGUAGE OverloadedStrings #-}

module MOO.Builtins.Hash (hashBytesUsing) where

import Crypto.Hash
  ( HashFunctionBS, hash
  , MD2, MD4, MD5
  , RIPEMD160
  , SHA1, SHA224, SHA256, SHA384, SHA512
--  , SHA3_224, SHA3_256, SHA3_384, SHA3_512
--  , Skein256_224, Skein256_256
--  , Skein512_224, Skein512_256, Skein512_384, Skein512_512
  , Tiger
  , Whirlpool
  )
import Data.ByteString (ByteString)
import Data.Map (Map)

import qualified Data.Map as M

import MOO.Types

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

hashBytesUsing :: Id -> ByteString -> Maybe String
hashBytesUsing alg bytes = ($ bytes) `fmap` M.lookup alg hashFunctions

hashFunctions :: Map Id (ByteString -> String)
hashFunctions = M.fromList algorithms
  where alias = (hashFunctions M.!)
        algorithms = [
            ("MD2"          , show . md2        )
          , ("MD4"          , show . md4        )
          , ("MD5"          , show . md5        )

          , ("RIPEMD-160"   , show . ripemd160  )
          , ("RIPEMD160"    , alias "RIPEMD-160")

          , ("SHA-1"        , show . sha1       )
          , ("SHA1"         , alias "SHA-1"     )

          , ("SHA-224"      , show . sha224     )
          , ("SHA-256"      , show . sha256     )
          , ("SHA-384"      , show . sha384     )
          , ("SHA-512"      , show . sha512     )

          , ("SHA224"       , alias "SHA-224"   )
          , ("SHA256"       , alias "SHA-256"   )
          , ("SHA384"       , alias "SHA-384"   )
          , ("SHA512"       , alias "SHA-512"   )
{-
  -- SHA-3 is not yet standard; these appear to implement Keccak variants
  , ("SHA3-224"     , show . (hash :: HashFunctionBS SHA3_224    ))
  , ("SHA3-256"     , show . (hash :: HashFunctionBS SHA3_256    ))
  , ("SHA3-384"     , show . (hash :: HashFunctionBS SHA3_384    ))
  , ("SHA3-512"     , show . (hash :: HashFunctionBS SHA3_512    ))

          -- I can't verify the correctness of this Skein implementation
          , ("Skein-256-224", show . skein256_224)
          , ("Skein-256-256", show . skein256_256)

          , ("Skein-512-224", show . skein512_224)
          , ("Skein-512-256", show . skein512_256)
          , ("Skein-512-384", show . skein512_384)
          , ("Skein-512-512", show . skein512_512)
-}
          , ("Tiger"        , show . tiger        )
          , ("Whirlpool"    , show . whirlpool    )
          ]

-- The hashing functions...

md2 :: HashFunctionBS MD2
md2 = hash

md4 :: HashFunctionBS MD4
md4 = hash

md5 :: HashFunctionBS MD5
md5 = hash

ripemd160 :: HashFunctionBS RIPEMD160
ripemd160 = hash

sha1 :: HashFunctionBS SHA1
sha1 = hash

sha224 :: HashFunctionBS SHA224
sha224 = hash

sha256 :: HashFunctionBS SHA256
sha256 = hash

sha384 :: HashFunctionBS SHA384
sha384 = hash

sha512 :: HashFunctionBS SHA512
sha512 = hash

{-
skein256_224 :: HashFunctionBS Skein256_224
skein256_224 = hash

skein256_256 :: HashFunctionBS Skein256_256
skein256_256 = hash

skein512_224 :: HashFunctionBS Skein512_224
skein512_224 = hash

skein512_256 :: HashFunctionBS Skein512_256
skein512_256 = hash

skein512_384 :: HashFunctionBS Skein512_384
skein512_384 = hash

skein512_512 :: HashFunctionBS Skein512_512
skein512_512 = hash
-}

tiger :: HashFunctionBS Tiger
tiger = hash

whirlpool :: HashFunctionBS Whirlpool
whirlpool = hash
