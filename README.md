# `kesha`

[![CI](https://github.com/jmackie/kesha/workflows/CI/badge.svg)](https://github.com/jmackie/kesha/actions?query=workflow%3ACI)
[![Hackage](https://img.shields.io/hackage/v/kesha)](https://hackage.haskell.org/package/kesha)

A Haskell library for computing the cryptographic hash of any path.

The implementation is an almost verbatim implementation of `nix-hash`, which is the
standard tool used by the [Nix](https://nixos.org/nix/) package manager.

```haskell
module Main where

import qualified Kesha

main :: IO ()
main = do
  result <- Kesha.hash "some-path"
  print result
```
