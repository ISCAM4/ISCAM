{
  description = "Opiniated Flake for Developing R Software--for ISCAM";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable"; # Unstable to get the latest version of R + packages. Pretty safe.
    flake-utils.url = "github:numtide/flake-utils";
    nix-vscode-extensions = {
      url = "github:nix-community/nix-vscode-extensions";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      nix-vscode-extensions,
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [ nix-vscode-extensions.overlays.default ];
        };

        # List the packages you wish to have installed instead of installing packages interactively
        # Useful links:
        # https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=rPackages.
        # https://nixos.wiki/wiki/R#Install_an_R-package_from_GitHub
        rPkgs = with pkgs.rPackages; [
          # ! NOTE: ADD YOUR R PACKAGES HERE
          devtools
          roxygen2
          testthat
          usethis
          pkgdown
          languageserver
          lintr
          withr
        ];

        rWithPkgs = pkgs.rWrapper.override {
          packages = rPkgs;
        };

        # Minimal open-vsx extensions
        # Note that positron-bin will probably lag behind stable release, so extensions which target the latest version of code-oss may not work.
        # e.g. on 20250817, vscode-pull-request-github-116.1 reports that it "is not compatible with Code 1.102.0. Extension requires: ^1.103.0."
        # https://nix-community.github.io/nix4vscode/
        extensions = with pkgs.open-vsx; [
          # Default Positron extensions
          github.vscode-pull-request-github
          posit.air-vscode
          quarto.quarto

          # ! NOTE: ADD YOUR OPEN-VSX EXTENSIONS HERE
          eamodio.gitlens
          usernamehw.errorlens
          edwinhuish.better-comments-next
          tomoki1207.pdf
          tombi-toml.tombi
          oderwat.indent-rainbow
          robole.marky-stats

          # Catppuccin theme extensions
          catppuccin.catppuccin-vsc
          catppuccin.catppuccin-vsc-icons
        ];

        # Declerative Positron settings.json
        # Clear settings by running: $ rm ~/.config/Positron/User/settings.json
        settingsJson = pkgs.writeText "settings.json" (
          builtins.toJSON {
            "workbench.keybindings.rstudioKeybindings" = true;
            "workbench.colorTheme" = "Catppuccin Mocha";
            "workbench.iconTheme" = "catppuccin-mocha";
            "workbench.list.smoothScrolling" = true;

            "catppuccin.accentColor" = "pink";

            "catppuccin.colorOverrides" = {
              "mocha" = {
                "base" = "#000000";
                "mantle" = "#010101";
                "crust" = "#020202";
              };
            };

            "catppuccin.customUIColors" = {
              "mocha" = {
                "statusBar.foreground" = "accent";
              };
            };

            "git.autofetch" = true;
            "git.confirmSync" = false;

            "editor.formatOnSave" = true;
            "editor.fontFamily" = "JetBrainsMono Nerd Font";
            "editor.fontLigatures" = true;
            "editor.semanticHighlighting.enabled" = true;
            "editor.pasteAs.preferences" = [
              "typst.link"
              "markdown.link"
            ];

            "terminal.integrated.fontFamily" = "IosevkaTerm Nerd Font";

            "positron.plots.darkFilter" = "off";

            "[r]" = {
              "editor.formatOnSave" = true;
              "editor.defaultFormatter" = "Posit.air-vscode";
            };
            "[quarto]" = {
              "editor.formatOnSave" = true;
              "editor.defaultFormatter" = "quarto.quarto";
            };
          }
        );

        # Symlink farm of extensions (read-only source)
        extensionsRO = pkgs.buildEnv {
          name = "positron-extensions";
          paths = extensions;
          pathsToLink = [ "/share/vscode/extensions" ];
        };

        # FHS environment for Positron
        positronFHS = pkgs.buildFHSEnv {
          name = "positron-fhs";
          targetPkgs =
            pkgs: with pkgs; [
              # Core Positron and R
              positron-bin
              R
              air-formatter

              # Patched fonts
              nerd-fonts.iosevka-term
              nerd-fonts.jetbrains-mono

              # Basic system libraries for dynamic linking
              glibc
              gcc-unwrapped.lib
              stdenv.cc.cc.lib
              zlib
              git
              gcc
              gfortran
              pkg-config
            ];

          runScript = pkgs.writeScript "positron-run" ''
            #!/bin/bash
            set -euo pipefail

            EXT_BASE="''${XDG_CACHE_HOME:-$HOME/.cache}"
            EXT_DST="$EXT_BASE/positron/extensions"
            SRC="${extensionsRO}/share/vscode/extensions"

            CONFIG_BASE="''${XDG_CONFIG_HOME:-$HOME/.config}"
            APP_ID="Positron"
            SETTINGS_DIR="$CONFIG_BASE/$APP_ID/User"
            SETTINGS_FILE="$SETTINGS_DIR/settings.json"

            mkdir -p "$EXT_DST" "$SETTINGS_DIR"

            # rm ~/.config/Positron/User/settings.json
            # Copy settings.json if it doesn't exist
            [ ! -f "$SETTINGS_FILE" ] && cp "${settingsJson}" "$SETTINGS_FILE"

            # Sync extensions (copy if not exists)
            for s in "$SRC"/*; do
              [ -d "$s" ] || continue
              bn="$(basename "$s")"
              [ -d "$EXT_DST/$bn" ] || cp -r "$s" "$EXT_DST/$bn"
            done

            # Set up environment
            export R_HOME='${pkgs.R}/lib/R'
            export R_LIBS_USER='${rWithPkgs}/lib/R/library'
            export PATH="${pkgs.R}/bin:${rWithPkgs}/bin:${pkgs.air-formatter}/bin:$PATH"

            # Run Positron
            exec positron --extensions-dir "$EXT_DST" "$@"
          '';
        };

        positronWrapper = pkgs.writeShellScriptBin "positron-with-extensions" ''
          exec ${positronFHS}/bin/positron-fhs "$@"
        '';

        rstudioWrapper = pkgs.writeShellScriptBin "rstudio-with-r" ''
          export R_HOME='${pkgs.R}/lib/R'
          export R_LIBS_USER='${rWithPkgs}/lib/R/library'
          export PATH="${pkgs.R}/bin:${rWithPkgs}/bin:${pkgs.air-formatter}/bin:$PATH"
          exec ${pkgs.rstudio}/bin/rstudio "$@"
        '';
      in
      {
        packages.positron = positronWrapper;
        packages.rstudio = rstudioWrapper;

        apps.positron = {
          type = "app";
          program = "${positronWrapper}/bin/positron-with-extensions";
          meta = {
            description = "Positron IDE with R environment and extensions";
            platforms = [ "x86_64-linux" ];
          };
        };

        apps.rstudio = {
          type = "app";
          program = "${rstudioWrapper}/bin/rstudio-with-r";
          meta = {
            description = "RStudio IDE with R environment";
            platforms = [ "x86_64-linux" ];
          };
        };

        devShells.default = pkgs.mkShell {
          shellHook = ''
            echo "Use: nix run .#positron or nix run .#rstudio"
          '';
        };
      }
    );
}
