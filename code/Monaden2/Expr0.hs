module Expr0 where

data Expr  = Const  Int
           | Binary BinOp Expr Expr
             deriving (Show)

data BinOp = Add | Sub | Mul | Div | Mod
             deriving (Eq, Show)


type Result = Int

eval :: Expr -> Result
eval (Const i)
    = i

eval (Binary op l r)
    = let f = lookupFtab op
      in
        f (eval l) (eval r)


type BinFct = Result -> Result -> Result

lookupFtab :: BinOp -> BinFct
lookupFtab op
    = case lookup op ftab of
        Nothing -> error
                   "operation not implemented"
        Just f  -> f

ftab :: [(BinOp, BinFct)]
ftab = [ (Add, (+))
       , (Sub, (-))
       , (Mul, (*))
       , (Div, div)
       ]


-- ----------------------------------------

{- some tests

e1 = Binary Mul (Binary Add (Const 2)
                            (Const 4)
                )
                (Const 7)
e2 = Binary Div (Const 1) (Const 0)
e3 = Binary Mod (Const 1) (Const 0)

v1 = eval e1
v2 = eval e2
v3 = eval e3

-- -}
-- ----------------------------------------
