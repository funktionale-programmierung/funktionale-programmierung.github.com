module Driver where

import System.Environment

driver computeSolutions =
    do [f] <- getArgs                -- Kommandozeilenargument abgreifen
       str <- readFile f             -- Datei einlesen
       let puzzles = lines str       -- Sudokurätsel aus `str` extrahieren
           solutions = computeSolutions puzzles
       print (length solutions)      -- Anzahl der Lösungen ausgeben
