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
        myGithubPages = pkgs.rubyPackages_2_7.github-pages;
      in {
        packages.default = pkgs.stdenv.mkDerivation {
          name = "build-blog";
          src = pkgs.lib.cleanSource ./.;
          buildInputs = [ myGithubPages ];
          installPhase = ''
            mkdir -p $out
            cp -r . $out
          '';
        };

        devShells.default =
          pkgs.mkShell { nativeBuildInputs = [ myGithubPages ]; };
      });
}
