{
  description = "lpfp book";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
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
            # pkgs.haskellPackages.ghcup
            pkgs.haskellPackages.gnuplot
            pkgs.haskellPackages.haskell-language-server
            pkgs.gnuplot
            pkgs.libGL
            pkgs.libGLU
            pkgs.freeglut
          ];

          # Add ghcup installation script
          shellHook = ''
            export PATH="${pkgs.curl}/bin:$PATH"

            if [ ! -f "$HOME/.ghcup/bin/ghcup" ]; then
              curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh
            fi
            export PATH="$HOME/.ghcup/bin:$PATH"
          '';
        };

        packages.default = pkgs.haskellPackages.callCabal2nix "lpfp-book-01" ./. { };
      }
    );
}
