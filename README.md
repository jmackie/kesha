# `kesha`

[![Build Status](https://travis-ci.org/jmackie/kesha.svg?branch=master)](https://travis-ci.org/jmackie/kesha)

A Haskell library and executable for computing the cryptographic hash of any path.

The implementation is an almost verbatim port of `nix-hash`, which is the
standard tool used by the [Nix](https://nixos.org/nix/) package manager.

# Installation

With [`cabal`](https://cabal.readthedocs.io/en/latest/)

```bash
cabal new-install
```

with [`stack`](https://docs.haskellstack.org/en/latest/)

```bash
stack install
```

with [Nix](https://nixos.org/nix/)

```bash
nix-env -f https://github.com/jmackie/kesha/archive/master.tar.gz -iA kesha
```
