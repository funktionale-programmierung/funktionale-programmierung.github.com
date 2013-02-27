-- Datei SudokuSeq.hs
import Sudoku
import System.Environment
import Data.Maybe

main =
    do [f] <- getArgs                -- Kommandozeilenargument abgreifen
       str <- readFile f             -- Datei einlesen
       let puzzles = lines str       -- Sudokurätsel aus `str` extrahieren
           solutions = computeSolutions puzzles
       print (length solutions)      -- Anzahl der Lösungen ausgeben
    where
      computeSolutions puzzles =
        filter isJust (map solve puzzles)
