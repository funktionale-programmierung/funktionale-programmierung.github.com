GHC = ghc
FLAGS = --make -O2 -threaded -rtsopts
compile = $(GHC) $(FLAGS)

all: seq par1

seq:
	$(compile) SudokuSeq.hs

par1:
	$(compile) SudokuPar1.hs

par1-ev:
	$(compile) -eventlog -o SudokuPar1_e SudokuPar1.hs

clean:
	rm -f SudokuSeq SudokuPar1 SudokuPar2 SudokuPar1_e *.hi *.o *.eventlog

par2:
	$(compile) SudokuPar2.hs

par3:
	$(compile) SudokuPar3.hs
