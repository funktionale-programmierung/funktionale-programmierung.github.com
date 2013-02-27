-- Datei SudokuPar2.hs
import Sudoku
import Driver

import Data.Maybe
import Control.Monad.Par

main = driver computeSolutions
    where
      computeSolutions puzzles =
          filter isJust (runPar (parMap solve puzzles))
