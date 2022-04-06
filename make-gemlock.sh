#!/usr/bin/env bash

nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/refs/tags/21.11.tar.gz \
	  -p bundler \
	  -p bundix \
	  --run 'bundler update; bundler lock; bundler package --no-install --path vendor; bundix; rm -rf vendor'

echo "You can now run nix-shell"
