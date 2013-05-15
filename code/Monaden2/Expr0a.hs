module Expr0a where

data Expr  = Const  Int
           | Binary BinOp Expr Expr
             deriving (Show)

data BinOp = Add | Sub | Mul | Div | Mod
             deriving (Eq, Show)


data Result a
           = Val { val :: a }
           | Exc { exc :: String }
             deriving (Show)

eval :: Expr -> Result Int
eval (Const i)
    = Val i

eval (Binary op l r)
    = case lookupFtab op of
        Exc s -> Exc s
        Val f ->
            case eval l of
              Exc s -> Exc s
              Val x ->
                  case eval r of
                    Exc s -> Exc s
                    Val y -> f x y

type BinFct = Int -> Int -> Result Int

lookupFtab :: BinOp -> Result BinFct
lookupFtab op
    = case lookup op ftab of
        Nothing -> Exc
                   "operation not implemented"
        Just f  -> Val f

ftab :: [(BinOp, BinFct)]
ftab = [ (Add, lift2 (+))
       , (Sub, lift2 (-))
       , (Mul, lift2 (*))
       , (Div, div')
       ]
    where
      lift2 f = \ x y -> Val $ f x y

div' :: Int -> Int -> Result Int
div' x y
    | y == 0    = Exc "division by zero"
    | otherwise = Val $ x `div` y

-- ----------------------------------------

-- {- some tests

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
