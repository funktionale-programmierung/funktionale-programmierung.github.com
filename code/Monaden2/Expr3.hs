module Expr3 where

import           Control.Monad (liftM2)

data Expr  = Const  Int
           | Binary BinOp Expr Expr
             deriving (Show)

data BinOp = Add | Sub | Mul | Div | Mod
           | PlusMinus
             deriving (Eq, Show)

newtype Result a
           = Val { val :: [a] }
             deriving (Show)

instance Monad Result where
    return x       = Val [x]
    (Val vs) >>= g = Val $
                     concat [val (g x) | x <- vs]

eval :: Expr -> Result Int
eval (Const i)
    = return i

eval (Binary op l r)
    = do f <- lookupFtab op
         f (eval l) (eval r)

type BinFct = Result Int -> Result Int -> Result Int

lookupFtab :: BinOp -> Result BinFct
lookupFtab op
    = case lookup op ftab of
        Nothing -> error
                   "operation not implemented"
        Just f  -> return f

ftab :: [(BinOp, BinFct)]
ftab = [ (Add,       liftM2 (+))
       , (Sub,       liftM2 (-))
       , (Mul,       liftM2 (*))
       , (Div,       div'')
       , (PlusMinus, plusMinus)
       ]

div''   :: BinFct
div'' x y
    = do x1 <- x
         y1 <- y
         if y1 == 0
           then error "division by zero"
           else return (x1 `div` y1)

plusMinus :: BinFct
plusMinus (Val xs1) (Val xs2)
    = Val $ concat [ [x1 + x2, x1 - x2]
                   | x1 <- xs1
                   , x2 <- xs2
                   ]

interval :: Int -> Int -> Expr
interval mid rad
    = Binary PlusMinus (Const mid) (Const rad)

{- some tests

e1 = Binary Mul (Binary Add (Const 2)
                            (Const 4)
                )
                (Const 7)
e2 = Binary Div (Const 1) (Const 0)
e3 = Binary Mod (Const 1) (Const 0)

i1 = interval 1 1
i2 = interval 6 2

e4 = Binary Mul i1 (Const 7)
e5 = Binary Add i1 i2
e6 = Binary PlusMinus e4 e5
e7 = Binary Div i2 i1

-- -}
