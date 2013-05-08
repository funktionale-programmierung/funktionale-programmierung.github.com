#!/bin/bash

if [ -z "$1" ]
then
    echo "USAGE: $0 HOST"
    exit 1
fi

./Main "$1" 500 1
