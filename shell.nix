{ pkgs ? import <nixpkgs> {} }:
pkgs.haskell.packages.ghc864.developPackage {
  root = ./.;
  name = "kesha";
}
