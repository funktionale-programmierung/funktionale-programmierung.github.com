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
        ruby = pkgs.ruby;
        env = pkgs.bundlerEnv {
          name = "fp-blog";
          inherit ruby;
          gemfile = ./Gemfile;
          lockfile = ./Gemfile.lock;
          gemset = ./gemset.nix;
        };
        fp-blog-bin = pkgs.writeScriptBin "fp-blog" ''
          exec ${env}/bin/jekyll $@
        '';
      in {
        # nix develop -c bash -c 'jekyll serve -c --watch'
        packages = {
          fp-blog = pkgs.stdenv.mkDerivation {
            name = "fp-blog";
            src = ./.;
            buildInputs = [ env ];
            nativeBuildInputs = [ fp-blog-bin ];
            installPhase = ''
              mkdir -p $out
              cp -r $src $out
            '';
          };
        };
        apps = {
          default = {
            type = "app";
            program = "exec ${
                self.packages.${system}.fp-blog
              }/bin/fp-blog serve -w --incremental";
          };
        };

        devShells = {
          default = pkgs.mkShell {
            nativeBuildInputs =
              [ env fp-blog-bin pkgs.bundler pkgs.ruby pkgs.bundix ];
          };
        };
      });
}
