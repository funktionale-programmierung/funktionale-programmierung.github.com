{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import Test.Framework.TestManager
import System.Environment

-- fehlerhafte reverse-Funktion
myReverse :: [a] -> [a]
myReverse []     = []
myReverse [x]    = [x]
myReverse (x:xs) = myReverse xs

test_nonEmpty =
    do assertEqual [1] (myReverse [1])
       assertEqual [3,2,1] (myReverse [1,2,3])

test_empty = assertEqual ([] :: [Int]) (myReverse [])

prop_reverse :: [Int] -> Bool
prop_reverse xs = xs == (myReverse (myReverse xs))

main =
    do args <- getArgs
       runTestWithArgs args htf_thisModulesTests
