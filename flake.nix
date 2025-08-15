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
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        rPackages = pkgs.rWrapper.override {
          packages = with pkgs.rPackages; [
            devtools
            roxygen2
            testthat
            usethis
            pkgdown
            languageserver
          ];
        };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            R
            rPackages
            positron-bin
            gcc
            gfortran
            pkg-config
          ];

          shellHook = ''
            export R_HOME=${pkgs.R}/lib/R
            export R_USER=${rPackages}/lib/R/library
            export PATH=${pkgs.R}/bin:${rPackages}/bin:$PATH
            export R_LIBS_USER=${rPackages}/lib/R/library
            export R_LIBS_SITE=${rPackages}/lib/R/library
          '';
        };
      }
    );
}
