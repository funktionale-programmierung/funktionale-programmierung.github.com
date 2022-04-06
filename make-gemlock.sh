#!/usr/bin/env bash

nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/refs/tags/21.11.tar.gz \
	  -p bundler \
	  -p bundix \
	  --run 'bundler update; bundler lock --add-platform x86_64-linux --add-platform x86_64-darwin; bundler package --no-install --path vendor; bundix; rm -rf vendor'

echo "You can now run nix-shell"
