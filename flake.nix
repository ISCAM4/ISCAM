{
  description = "Minimal reproducible R dev environment (no workspace save, no history)";

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
        pkgs = import nixpkgs { inherit system; };
        Rnosave = pkgs.writeShellScriptBin "R" (
          let
            r = pkgs.rWrapper.override {
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
          ''
            exec ${r}/bin/R --no-save --no-restore-data --no-restore-history "$@"
          ''
        );
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            Rnosave
            pkgs.gcc
            pkgs.gfortran
            pkgs.pkg-config
          ];
          shellHook = ''
            export R_LIBS_USER="$PWD/.Rlibs"
            mkdir -p "$R_LIBS_USER"
            export R_HISTFILE=/dev/null
          '';
        };
      }
    );
}
