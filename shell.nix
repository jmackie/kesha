{ compiler ? "ghc865", dev ? true # Include tools useful for development?
}:
let
  pkgs = let rev = "7d75a77954aaa61fa0b2931b354b61cc0aa4a60a";
  in import (builtins.fetchTarball {
    name = "nixpkgs-${rev}";
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "0qf3rp0jqgipjzz51ws8nwxiaiglz9jf7hc6d7x75ddbp0wlfjmg";
  }) { overlays = [ (self: super: { llvm_39 = super.llvm_5; }) ]; };

  old-ghc-nix = let rev = "674d459c7376af6641c94e91cd7d36214661b481";
  in import (builtins.fetchTarball {
    name = "old-ghc-nix-${rev}";
    url = "https://github.com/mpickering/old-ghc-nix/archive/${rev}.tar.gz";
    sha256 = "1w3d5dm9v7ms1s7xp39wncgwvh3kclp2bkdqa2kxn3x5dyayfm61";
  }) { inherit pkgs; };

in pkgs.mkShell {
  buildInputs =
    [ old-ghc-nix."${compiler}" pkgs.cabal-install pkgs.ormolu pkgs.hlint ]
    ++ (if dev then [ pkgs.ghcid ] else [ ]);
}
