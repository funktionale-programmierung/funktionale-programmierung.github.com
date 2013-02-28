#!/bin/bash

if [ -z "$1" ]
then
    echo "USAGE: $0 FILE"
    exit 1
fi

IN="$1"

make all

function run()
{
    cmd="./$1 +RTS -s $2 -RTS $IN"
    echo $cmd
    $cmd 2>&1 | grep '^  Total'
}

run SudokuSeq
run SudokuDummy

# run SudokuPar1
# run SudokuPar1 -N2
# run SudokuPar1 -N4

run SudokuPar2
run SudokuPar2 -N2
run SudokuPar2 -N4
run SudokuPar2 -N8
run SudokuPar2 -N12
run SudokuPar2 -N16
