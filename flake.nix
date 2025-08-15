{
  description = "R package development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
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
        pkgs = nixpkgs.legacyPackages.${system};

        rEnv = pkgs.rWrapper.override {
          packages = with pkgs.rPackages; [
            devtools
            roxygen2
            testthat
            usethis
            pkgdown
          ];
        };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            rEnv
            gcc
            gfortran
            pkg-config
          ];
        };
      }
    );
}
