let
  pinnedPkgs =
    # 2020-05-17T11:55:13+02:00
    let rev = "85d6f3bcd9cbcc52c4a307d2ef5116dab4b41641";
    in import (builtins.fetchTarball {
      name = "nixpkgs-${rev}";
      url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
      sha256 = "10jxpwq47clbj047jh5zn20hqmwpc821ac8zljgpayk0sk5p0mwv";
    }) { };
in { pkgs ? pinnedPkgs, compiler ? "ghc865" }:
pkgs.mkShell {
  buildInputs = [
    pkgs.haskell.packages."${compiler}".ghc
    pkgs.cabal-install
    pkgs.ormolu
    pkgs.hlint
  ];
}

