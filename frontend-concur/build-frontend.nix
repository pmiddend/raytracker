let
  nixpkgs = import (builtins.fetchTarball {
    name = "nixos-unstable-2021-11-20";
    url = "https://github.com/nixos/nixpkgs/archive/a03719be2e676041847e1b59dafa8ab668fa2d2a.tar.gz";
    sha256 = "1xvcc89m80cpslv84qhbbcyvcjqxhip5drx1arxvaajqv9059nqk";
  }) {};
  nodeDependencies = (nixpkgs.callPackage ({ pkgs, system }:
    let nodePackages = import ./default.nix { inherit pkgs system; };
    in nodePackages // {
      shell = nodePackages.shell.override {
        buildInputs = [ pkgs.nodePackages.node-gyp-build ];
      };
    }
  ) {}).shell.nodeDependencies;
  easy-ps = import
    (nixpkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "7802db65618c2ead3a55121355816b4c41d276d9";
      sha256 = "0n99hxxcp9yc8yvx7bx4ac6askinfark7dnps3hzz5v9skrvq15q";
    }) {
    pkgs = nixpkgs;
    };
  spagoPkgs = import ./spago-packages.nix { pkgs = nixpkgs; };
in

nixpkgs.stdenv.mkDerivation {
  name = "raytracker-frontend-concur";
  src = ./.;
  buildInputs = [
    nixpkgs.yarn
    easy-ps.purs-0_14_5
    easy-ps.spago
    spagoPkgs.installSpagoStyle
    spagoPkgs.buildSpagoStyle
    spagoPkgs.buildFromNixStore
  ];
  buildPhase = ''
    install-spago-style
    build-spago-style src/*purs src/Raytracker/*purs

    ln -s ${nodeDependencies}/lib/node_modules ./node_modules
    export PATH="${nodeDependencies}/bin:$PATH"

    ./node_modules/.bin/parcel --version
    yarn run --offline parcel build index.html
  '';
  installPhase = ''
    mkdir -p $out
    cp dist/* $out/
  '';
}
