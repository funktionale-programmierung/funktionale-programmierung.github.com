#!/bin/bash

if [ "$1" == "--prof" ]
then
    ghc -threaded --make -prof -auto-all -caf-all -o Main_p -O2 Main.hs
else
    ghc -rtsopts -threaded -fwarn-unused-imports --make -O2 Main.hs
    ghc -rtsopts -threaded -fwarn-unused-imports --make -O2 MainConduit.hs
fi
