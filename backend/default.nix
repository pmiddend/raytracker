let nixpkgs = import (builtins.fetchTarball {
  name = "nixos-unstable-2021-11-20";
  url = "https://github.com/nixos/nixpkgs/archive/a03719be2e676041847e1b59dafa8ab668fa2d2a.tar.gz";
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "1xvcc89m80cpslv84qhbbcyvcjqxhip5drx1arxvaajqv9059nqk";
    }) {};
    compiler = "ghc8107";
in
  nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./cabal2nix.nix { }
