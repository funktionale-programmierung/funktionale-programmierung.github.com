{
  description = "Commands for developing the funktionale-programmierung.de website";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        jekyllFull = pkgs.jekyll.override {
          # this way jekyll knows all the necessary plugins
          withOptionalDependencies = true;
        };
      in {
        packages = {
          default = pkgs.stdenv.mkDerivation {
            name = "build-blog";
            src = pkgs.lib.cleanSource ./.;
            buildInputs = [ jekyllFull ];
            installPhase = ''
              mkdir -p $out
              cp -r . $out
            '';
          };
          serveBlog = pkgs.writeShellScriptBin "serve-blog"
            "${pkgs.lib.getExe jekyllFull} serve --watch --incremental";
        };

        apps.default =
          flake-utils.lib.mkApp { drv = self.packages.${system}.serveBlog; };

        devShells.default =
          pkgs.mkShell { nativeBuildInputs = [ jekyllFull ]; };
      });
}
