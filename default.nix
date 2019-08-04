{ pkgs ? import <nixpkgs> {}
, compiler ? "ghc864"
, returnShellEnv ? pkgs.lib.inNixShell
}:

let
  haskellPackages =
    pkgs.haskell.packages.${compiler}.override {
      overrides = self: super: {
      };
    };

  gitignore = (import (pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore";
    rev = "ec5dd0536a5e4c3a99c797b86180f7261197c124";
    sha256 = "0k2r8y21rn4kr5dmddd3906x0733fs3bb8hzfpabkdav3wcy3klv";
  }) { inherit (pkgs) lib; }).gitignoreSource;

  drv =
    (haskellPackages.callCabal2nix "kesha" (gitignore ./.) {}).overrideAttrs
      # Need `nix-*` tools for testing
      (attrs: { buildInputs = attrs.buildInputs ++ [ pkgs.nix ]; });

  env = haskellPackages.shellFor {
    packages = p: [ drv ];
    buildInputs = [];
  };
in

if returnShellEnv then env else drv
