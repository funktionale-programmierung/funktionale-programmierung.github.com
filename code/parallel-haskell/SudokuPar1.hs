-- Datei SudokuPar1.hs
import Sudoku
import System.Environment
import Data.Maybe
import Control.Monad.Par

main =
    do [f] <- getArgs                -- Kommandozeilenargument abgreifen
       str <- readFile f             -- Datei einlesen
       length str `seq` return ()
       let puzzles = lines str       -- Sudokurätsel aus `str` extrahieren
           solutions = computeSolutions puzzles
       print (length solutions)      -- Anzahl der Lösungen ausgeben
    where
      computeSolutions puzzles =
          let (as, bs) = splitAt (length puzzles `div` 2) puzzles
          in runPar (do i1 <- new
                        i2 <- new
                        fork (put i1 (map solve as))
                        fork (put i2 (map solve bs))
                        res1 <- get i1
                        res2 <- get i2
                        return (res1 ++ res2))
