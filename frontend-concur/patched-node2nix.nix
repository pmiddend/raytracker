let pkgs = import <nixpkgs> {};
in pkgs.nodePackages.node2nix.override {
  src = pkgs.fetchFromGitHub {
          owner = "thomasjm";
          repo = "node2nix";
          rev = "master";
          sha256 = "sha256-S1a87M3NHOXgAFDNVi2vXRbpDKZ/yZy9sUtwutDZBz0=";
        };
}
