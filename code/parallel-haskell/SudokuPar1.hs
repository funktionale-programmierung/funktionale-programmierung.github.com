-- Datei SudokuPar1.hs
import Sudoku
import Driver

import Data.Maybe
import Control.Monad.Par

main = driver computeSolutions
    where
      computeSolutions puzzles =
          let (as, bs) = splitAt (length puzzles `div` 2) puzzles
          in runPar (do b1 <- new
                        b2 <- new
                        fork (put b1 (map solve as))
                        fork (put b2 (map solve bs))
                        res1 <- get b1
                        res2 <- get b2
                        return (filter isJust (res1 ++ res2)))
