{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Kesha
-- Copyright: (c) 2020 Jordan Mackie
-- License: MIT
-- Maintainer: Jordan Mackie <contact@jmackie.dev>
-- Stability: experimental
-- Portability: portable
--
-- An implementation of @<https://nixos.org/guides/nix-pills/nix-store-paths.html#idm140737319621872 nix-hash>@.
module Kesha
  ( hash,
    hashWith,
    HashOptions (..),
    defaultHashOptions,
    HashAlgo (..),
    HashRepr (..),
  )
where

import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as ASCII
import qualified Data.Char as Char
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import Data.Word (Word8)
import qualified Kesha.NAR as NAR
import Prelude hiding ((!!))

-- |
-- Compute the cryptographic hash of a path using the 'defaultHashOptions'.
--
-- The output of @'hash' path@ should be consistent with that of
-- @nix-hash --type sha256 --base32 path@.
hash :: FilePath -> IO (Either NAR.PackError BS.ByteString)
hash = hashWith defaultHashOptions

-- |
-- Compute the cryptographic hash of a path using the given 'HashOptions'.
hashWith :: HashOptions -> FilePath -> IO (Either NAR.PackError BS.ByteString)
hashWith opts path =
  fmap (printNar (hashAlgo opts) (hashRepr opts)) <$> NAR.localPack path

-- |
-- Hashing options.
data HashOptions = HashOptions
  { -- | cryptographic hash algorithm to use
    hashAlgo :: HashAlgo,
    -- | how to print the hash
    hashRepr :: HashRepr
  }

-- |
-- Default hashing options.
--
-- These are the default options used by most of the Nix tooling (e.g.
-- @nix-prefetch-git@).
defaultHashOptions :: HashOptions
defaultHashOptions = HashOptions SHA256 Base32

-- |
-- Available hash algorithms.
data HashAlgo
  = MD5
  | SHA1
  | SHA256

-- |
-- Printable hash representations.
data HashRepr
  = Base16
  | Base32

printNar :: HashAlgo -> HashRepr -> NAR.NAR -> BS.ByteString
printNar algo repr =
  ASCII.map Char.toLower
    . ( case repr of
          Base16 -> printHash16 algo
          Base32 -> printHash32 algo
      )
    . ( case algo of
          MD5 -> MD5.hash
          SHA1 -> SHA1.hash
          SHA256 -> SHA256.hash
      )
    . NAR.dump

-- https://github.com/NixOS/nix/blob/master/src/libutil/hash.cc
printHash16 :: HashAlgo -> BS.ByteString -> BS.ByteString
printHash16 algo rawHash =
  ASCII.pack $
    foldMap
      ( \i ->
          [ base16Chars !! fromIntegral (BS.index rawHash i `shiftR` 4),
            base16Chars !! fromIntegral (BS.index rawHash i .&. 15)
          ]
      )
      [0 .. hashSize - 1]
  where
    hashSize :: Int
    hashSize = hashSizeForAlgo algo

    base16Chars :: Seq.Seq Char
    base16Chars = "0123456789abcdef"

-- https://github.com/NixOS/nix/blob/master/src/libutil/hash.cc
printHash32 :: HashAlgo -> BS.ByteString -> BS.ByteString
printHash32 algo rawHash = go (len - 1) ""
  where
    hashSize :: Int
    hashSize = hashSizeForAlgo algo

    -- omitted: E O U T
    base32Chars :: Seq.Seq Char
    base32Chars = Seq.fromList "0123456789abcdfghijklmnpqrsvwxyz"

    len :: Int
    len = (hashSize * 8 - 1) `div` 5 + 1

    go :: Int -> BS.ByteString -> BS.ByteString
    go n accum
      | n < 0 = accum
      | otherwise =
        go (pred n) $
          ASCII.snoc accum (base32Chars !! (fromIntegral c .&. 0x1f))
      where
        b, i, j :: Int
        b = n * 5
        i = b `div` 8
        j = b `mod` 8
        c :: Word8
        c =
          ((bytes !! i) `shiftR` j)
            .|. (if i >= (hashSize - 1) then 0 else (bytes !! (i + 1)) `shiftL` (8 - j))

    bytes :: Seq.Seq Word8
    bytes = Seq.fromList (BS.unpack rawHash)

-- https://github.com/NixOS/nix/blob/master/src/libutil/hash.hh
hashSizeForAlgo :: HashAlgo -> Int
hashSizeForAlgo MD5 = 16
hashSizeForAlgo SHA1 = 20
hashSizeForAlgo SHA256 = 32

(!!) :: Seq.Seq a -> Int -> a
(!!) xs i = fromJust (Seq.lookup i xs)

infixl 9 !!
