{
  description = "lpfp book";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.haskellPackages.ghc
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.ghcup
            pkgs.haskellPackages.gnuplot
            pkgs.gnuplot
          ];
        };

        packages.default = pkgs.haskellPackages.callCabal2nix "lpfp-book-01" ./. {};
      });
}