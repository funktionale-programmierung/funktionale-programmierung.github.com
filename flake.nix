{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        myRubyPackages = pkgs.rubyPackages_2_7;
        myGithubPages = myRubyPackages.github-pages;
      in {
        packages = {
          default = self.packages.${system}.buildBlog;
          buildBlog = pkgs.stdenv.mkDerivation {
            name = "build-blog";
            src = pkgs.lib.cleanSource ./.;
            buildInputs = [ myGithubPages ];
            buildPhase = ''
              set -x
              jekyll build
            '';
            installPhase = ''
              mkdir -p $out
              cp -r . $out
            '';
          };
          serveBlog = pkgs.writeShellScriptBin "serve-blog.sh"
            ''
               ${pkgs.lib.getExe myRubyPackages.jekyll} serve --watch
            '';
        };

        apps = {
          default = flake-utils.lib.mkApp { drv = self.packages.${system}.serveBlog; };
        };

        devShells = {
          default = pkgs.mkShell {
            nativeBuildInputs =
              [ myGithubPages self.packages.${system}.serveBlog ];
          };
        };
      });
}
