# `kesha`

Generate a `sha256` hash for any filesystem thing.

Until I get round to writing this properly, the idea is that

```
cabal new-build
cabal new-run kesha -- ./src

# Or with stack
stack build
stack run kesha -- ./src
```

Matches...

```
nix-hash --type sha256 --base32 ./src
```

Note that the flags to `nix-hash` are the default behaviour for
`nix-prefetch-git`.
