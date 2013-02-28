-- Datei SudokuPar2.hs
import Sudoku
import Driver

import Data.Maybe
import Control.Monad.Par

main = driver computeSolutions
    where
      computeSolutions puzzles =
          filter isJust (runPar (myParMap solve puzzles))

myParMap f xs =
    do bs <- mapM g xs
       mapM get bs
    where
      g x =
          do b <- new
             fork (put b (f x))
             return b
