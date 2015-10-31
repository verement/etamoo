
{-# LANGUAGE OverloadedStrings #-}

module MOO.Builtins.Hash (hashBytesUsing) where

import Crypto.Hash
  ( HashAlgorithm, Digest, hash
  , MD2(MD2), MD4(MD4), MD5(MD5)
  , RIPEMD160(RIPEMD160)
  , SHA1(SHA1), SHA224(SHA224), SHA256(SHA256), SHA384(SHA384), SHA512(SHA512)
  , SHA3_224(SHA3_224), SHA3_256(SHA3_256), SHA3_384(SHA3_384), SHA3_512(SHA3_512)
  , Skein256_224(Skein256_224), Skein256_256(Skein256_256)
  , Skein512_224(Skein512_224), Skein512_256(Skein512_256)
  , Skein512_384(Skein512_384), Skein512_512(Skein512_512)
  , Tiger(Tiger)
  , Whirlpool(Whirlpool)
  )
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.Map (Map)

import qualified Data.Map as M

import MOO.Types

import qualified MOO.String as Str

hashBytesUsing :: Id -> Bool -> ByteString -> Maybe StrT
hashBytesUsing algorithm wantBinary bytes =
  M.lookup algorithm hashFunctions >>= \f -> return (f wantBinary bytes)

type HashFunction = Bool -> ByteString -> StrT

hashFunctions :: Map Id HashFunction
hashFunctions = M.fromList algorithms

  where algorithms :: [(Id, HashFunction)]
        algorithms =
          [ ("MD2"          , hashWith MD2         )
          , ("MD4"          , hashWith MD4         )
          , ("MD5"          , hashWith MD5         )

          , ("RIPEMD-160"   , hashWith RIPEMD160   )
          , ("RIPEMD160"    , alias "RIPEMD-160"   )

          , ("SHA-1"        , hashWith SHA1        )
          , ("SHA1"         , alias "SHA-1"        )

          , ("SHA-224"      , hashWith SHA224      )
          , ("SHA-256"      , hashWith SHA256      )
          , ("SHA-384"      , hashWith SHA384      )
          , ("SHA-512"      , hashWith SHA512      )

          , ("SHA224"       , alias "SHA-224"      )
          , ("SHA256"       , alias "SHA-256"      )
          , ("SHA384"       , alias "SHA-384"      )
          , ("SHA512"       , alias "SHA-512"      )

          , ("SHA3-224"     , hashWith SHA3_224    )
          , ("SHA3-256"     , hashWith SHA3_256    )
          , ("SHA3-384"     , hashWith SHA3_384    )
          , ("SHA3-512"     , hashWith SHA3_512    )

          , ("SHA-3-224"    , alias "SHA3-224"     )
          , ("SHA-3-256"    , alias "SHA3-256"     )
          , ("SHA-3-384"    , alias "SHA3-384"     )
          , ("SHA-3-512"    , alias "SHA3-512"     )

          , ("Skein-256-224", hashWith Skein256_224)
          , ("Skein-256-256", hashWith Skein256_256)

          , ("Skein-512-224", hashWith Skein512_224)
          , ("Skein-512-256", hashWith Skein512_256)
          , ("Skein-512-384", hashWith Skein512_384)
          , ("Skein-512-512", hashWith Skein512_512)

          , ("Tiger"        , hashWith Tiger       )
          , ("Whirlpool"    , hashWith Whirlpool   )
          ]

        alias :: Id -> HashFunction
        alias = (hashFunctions M.!)

hashWith :: HashAlgorithm a => a -> Bool -> ByteString -> StrT
hashWith algorithm wantBinary = mkResult . hash' algorithm

  where hash' :: HashAlgorithm a => a -> ByteString -> Digest a
        hash' _ = hash

        mkResult :: Digest a -> StrT
        mkResult | wantBinary = Str.fromBinary . convert
                 | otherwise  = Str.fromString . show
