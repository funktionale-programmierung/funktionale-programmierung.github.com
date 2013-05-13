module Expr1 where

data Expr  = Const  Int
           | Binary BinOp Expr Expr
             deriving (Show)

data BinOp = Add | Sub | Mul | Div | Mod
             deriving (Eq, Show)

newtype Result a
    = Val { val :: a }
      deriving (Show)

instance Monad Result where
    return        = Val
    (Val v) >>= g = g v


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
ftab  = [ (Add, liftM2 (+))
        , (Sub, liftM2 (-))
        , (Mul, liftM2 (*))
        , (Div, liftM2 div)
        ]

liftM2  :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 f m1 m2
    = do x1 <- m1
         x2 <- m2
         return (f x1 x2)

-- ----------------------------------------

{- some tests

e1 = Binary Mul (Binary Add (Const 2)
                            (Const 4)
                )
                (Const 7)
e2 = Binary Div (Const 1) (Const 0)
e3 = Binary Mod (Const 1) (Const 0)

v1 = eval e1

-- -}
-- ----------------------------------------
