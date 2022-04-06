with import <nixpkgs> { };

let
  jekyll_env = bundlerEnv {
    name = "jekyll_env";
    inherit ruby;
    gemfile = ./Gemfile;
    lockfile = ./Gemfile.lock;
    gemset = ./gemset.nix;
  };
in stdenv.mkDerivation rec {
  name = "funktionale-programmierung.github.com";
  buildInputs = [ jekyll_env bundler ruby ];
  shellHook = ''
    exec ${jekyll_env}/bin/jekyll serve --watch
  '';
}
