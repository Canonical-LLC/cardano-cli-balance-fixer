{
  description = "A haskell.nix flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      # workaround for nix 2.6.0 bug
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.follows = "haskellNix/nixpkgs-2111";
  };

  outputs = { self, flake-utils, haskellNix, nixpkgs, ... }:
    let
      inherit (haskellNix) config;
      overlays = [
        haskellNix.overlay
        (final: prev: rec {
          cardano-cli-balance-fixer-project = haskell-nix.project {
            compiler-nix-name = "ghc8107";

            src = haskell-nix.haskellLib.cleanGit {
              name = "cardano-cli-balance-fixer-src";
              src = ../..;
            };
            index-state = "2022-03-15T00:00:00Z";
          };
        })
      ];
    in
    flake-utils.lib.eachSystem (import ./supported-systems.nix)
      (system:
        let
          pkgs = import nixpkgs { inherit system config overlays; };
          flake = pkgs.cardano-cli-balance-fixer-project.flake { };
        in
        nixpkgs.lib.recursiveUpdate flake {
          # so `nix build` will build the exe
          defaultPackage = flake.packages."cardano-cli-balance-fixer:exe:cardano-cli-balance-fixer";

          # so `nix run`  will run the exe
          defaultApp = {
            type = "app";
            program = "${flake.packages."cardano-cli-balance-fixer:exe:cardano-cli-balance-fixer"}/bin/cardano-cli-balance-fixer";
          };

          legacyPackages = pkgs;
        }
      );
}
