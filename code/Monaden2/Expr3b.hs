{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Expr3a where

import           Control.Monad       (join, liftM2)
import           Control.Monad.Error (MonadError (..))
import           Data.List           (partition)

data Expr  = Const  Int
           | Binary BinOp Expr Expr
             deriving (Show)

data BinOp = Add | Sub | Mul | Div | Mod
           | PlusMinus
             deriving (Eq, Show)

data Result a
           = Val { val :: [a] }
           | Exc { exc :: String }
             deriving (Show)

instance Monad Result where
    return x       = Val [x]

    (Exc e) >>= _  = (Exc e)
    (Val xs) >>= g = if not (null vs)
                     then Val $ concat . map val $ vs
                     else head es
        where
          (vs, es) = partition isVal . map g $ xs
              where
                isVal (Val _) = True
                isVal (Exc _) = False

instance MonadError String Result where
    throwError = Exc
    catchError (Exc e) handler
        = handler e
    catchError v _handler
        = v

eval :: Expr -> Result Int
eval (Const i)
    = return i

eval (Binary op l r)
    = do mf <- lookupMft op
         mf (eval l) (eval r)

type MF = Result Int -> Result Int -> Result Int

lookupMft :: BinOp -> Result MF
lookupMft op
    = case lookup op mft of
        Nothing -> throwError
                   "operation not implemented"
        Just mf -> return mf

mft :: [(BinOp, MF)]
mft = [ (Add, liftM2 (+))
      , (Sub, liftM2 (-))
      , (Mul, liftM2 (*))
      , (Div, \ x y -> join  (liftM2 div' x y))
      , (PlusMinus, plusMinus)
      ]

div' :: Int -> Int -> Result Int
div' x y
  | y == 0    = throwError
                "division by zero"
  | otherwise = return (x `div` y)

plusMinus :: MF
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
