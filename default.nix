let
  cabal-hashes-overlay =
    # Update from Hackage at 2019-08-05T11:50:23Z
    let rev = "ef12e2cf418a00852f174a68c33b907ee57e750f"; in
    self: super:  {
      all-cabal-hashes = super.fetchurl {
        url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/${rev}.tar.gz";
        sha256 = "1rspbkmj0369y5hw81k3y9ns2v2bzsnqpi4dhnqd0qxxzv3n1am7";
      };
    };

  pkgs =
    # 2019-08-05
    let rev = "621880e5e761960d910b443ff9788c88baddc4f9"; in
    import (builtins.fetchTarball {
      name = "nixpkgs-${rev}";
      url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
      sha256 = "1jzhc0k07cy37sravy0s9mmwrddy0vnp3479d8ndk750ncb2sjx1";
    }) { overlays = [ cabal-hashes-overlay ]; };

  haskellPackages =
    pkgs.haskell.packages.ghc865.override {
      overrides = self: super: {
      };
    };

  gitignore =
    (import (pkgs.fetchFromGitHub {
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

if pkgs.lib.inNixShell then env else drv
