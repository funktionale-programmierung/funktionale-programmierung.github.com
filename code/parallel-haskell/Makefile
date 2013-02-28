GHC = ghc
FLAGS = --make -O2 -threaded -rtsopts
compile = $(GHC) $(FLAGS)

.PHONY: all seq par1 par1-ev clean par2 par2-ev par2-my dummy archive

all: seq par1 par2 dummy

seq:
	$(compile) SudokuSeq.hs

par1:
	$(compile) SudokuPar1.hs

par1-ev:
	$(compile) -eventlog -o SudokuPar1_e SudokuPar1.hs

clean:
	rm -f SudokuSeq SudokuPar1 SudokuPar2 Sudoku*_e *.hi *.o *.eventlog SudokuDummy SudokuPar2OwnParMap

par2:
	$(compile) SudokuPar2.hs

par2-ev:
	$(compile) -eventlog -o SudokuPar2_e SudokuPar2.hs

par2-my:
	$(compile) SudokuPar2OwnParMap.hs

dummy:
	$(compile) SudokuDummy.hs

archive: clean
	cd .. && zip ../files/parallel-haskell/parallel-haskell.zip parallel-haskell/*
