{-# LANGUAGE OverloadedStrings #-}
module Kesha
  ( hash
  , hashWith

  , Opts(..)
  , defaultOpts
  , HashAlgo(..)
  , HashRepr(..)
  ) where

import Prelude hiding ((!!))

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as ASCII
import qualified Data.Char as Char
import qualified Data.Hex as Hex
import qualified Data.Sequence as Seq

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Maybe (fromJust)
import Data.Word (Word8)

import qualified Kesha.NAR as NAR

-- | 
-- Compute the cryptographic hash of a path using the 'defaultOpts'.
--
-- The output of @'hash' path@ should be consistent with that of 
-- @nix-hash --type sha256 --base32 path@ (invoked at the command line).
--
hash :: FilePath -> IO (Either String BS.ByteString)
hash = hashWith defaultOpts

-- | 
-- Compute the cryptographic hash of a path using the given 'Opts'.
--
hashWith :: Opts -> FilePath -> IO (Either String BS.ByteString)
hashWith opts path =
  fmap (printNar (hashAlgo opts) (hashRepr opts)) <$> NAR.localPack path

-- | 
-- Hashing options.
--
data Opts
  = Opts
      { hashAlgo :: HashAlgo -- ^ cryptographic hash algorithm to use
      , hashRepr :: HashRepr -- ^ how to print the hash
      }

-- |
-- Default hashing options.
--
-- These are the default options used by most of the Nix tooling (e.g.
-- @nix-prefetch-git@).
--
defaultOpts :: Opts
defaultOpts = Opts SHA256 Base32

-- |
-- Available hash algorithms.
--
data HashAlgo
  = SHA256

-- | 
-- Printable hash representations.
--
data HashRepr
  = Hex
  | Base32

printNar :: HashAlgo -> HashRepr -> NAR.NAR -> BS.ByteString
printNar SHA256 Hex
  = ASCII.map Char.toLower
  . Hex.hex
  . SHA256.hash
  . NAR.dump

printNar SHA256 Base32
  = ASCII.map Char.toLower
  . printHash32 SHA256
  . SHA256.hash
  . NAR.dump

-- https://github.com/NixOS/nix/blob/master/src/libutil/hash.cc
printHash32 :: HashAlgo -> BS.ByteString -> BS.ByteString
printHash32 algo rawHash = go (len - 1) ""
  where
    hashSize :: Int
    hashSize = hashSizeForAlgo algo

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
          c = ((bytes !! i) `shiftR` j) .|.
              (if i >= (hashSize - 1) then 0 else (bytes !! (i + 1)) `shiftL` (8 - j))

    bytes :: Seq.Seq Word8
    bytes = Seq.fromList (BS.unpack rawHash)

    (!!) :: Seq.Seq a -> Int -> a
    (!!) xs i = fromJust (Seq.lookup i xs)
    infixl 9 !!

-- https://github.com/NixOS/nix/blob/master/src/libutil/hash.hh
hashSizeForAlgo :: HashAlgo -> Int
hashSizeForAlgo SHA256 = 32

-- omitted: E O U T
base32Chars :: Seq.Seq Char
base32Chars = Seq.fromList "0123456789abcdfghijklmnpqrsvwxyz"
