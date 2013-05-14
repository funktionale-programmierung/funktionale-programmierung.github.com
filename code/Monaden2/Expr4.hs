{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Expr4 where

import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Reader

data Expr  = Const  Int
           | Var    Id
           | Let    Id    Expr Expr
           | Binary BinOp Expr Expr
             deriving (Show)

data BinOp = Add | Sub | Mul | Div | Mod
             deriving (Eq, Show)

type Id    = String


type Env   = [(Id, Int)]

data ResVal a
           = Val { val :: a }
           | Exc { exc :: String }
             deriving (Show)

newtype Result a
           = Res { unRes :: Env -> ResVal a }

instance Monad Result where
    return x      = Res $ \ _env -> Val x
    (Res f) >>= g = Res $ \ env ->
                    case f env of
                      (Exc e) -> (Exc e)
                      (Val v) -> unRes (g v) env

instance MonadError String Result where
    throwError s  = Res $ \ _env -> Exc s

    -- not yet used in expression evaluation
    catchError (Res f) handler
                = Res $ \ env ->
                  case f env of
                  (Exc e) -> unRes (handler e) env
                  x       -> x

instance MonadReader Env Result where
  ask       = Res $ \ env -> Val env
  local f c = Res $ \ env -> unRes c (f env)

eval :: Expr -> Result Int
eval (Const i)
    = return i

eval (Binary op l r)
    = do f <- lookupFtab op
         f (eval l) (eval r)

eval (Var ident)
    = do env <- ask
         case lookup ident env of
           Nothing -> throwError
                      $ "free variable '"
                         ++ ident
                         ++ "' found"
           Just v  -> return v

eval (Let ident e1 e2)
    = do v1 <- eval e1
         local (addEnv ident v1)
                   (eval e2)
    where
      addEnv i v = ((i, v) :)


type BinFct = Result Int -> Result Int -> Result Int

lookupFtab :: BinOp -> Result BinFct
lookupFtab op
    = case lookup op ftab of
        Nothing -> throwError
                   "operation not yet implemented"
        Just f  -> return f

ftab :: [(BinOp, BinFct)]
ftab = [ (Add, liftM2 (+))
       , (Sub, liftM2 (-))
       , (Mul, liftM2 (*))
       , (Div, \ x -> join . liftM2 div' x)
       ]

div' :: Int -> Int -> Result Int
div' x y
    | y == 0    = throwError
                  "division by zero"
    | otherwise = return (x `div` y)

runEval :: Expr -> Env -> ResVal Int
runEval e env
    = let (Res f) = eval e in
      f env

-- ----------------------------------------
{- tests

e1 = Binary Mul (Binary Add (Const 2)
                            (Const 4)
                )
                (Const 7)
e2 = Binary Div (Const 1) (Const 0)
e3 = Binary Mod (Const 1) (Const 0)

x  = Var "x"
y  = Var "y"

l1 = Binary Mul x y
l2 = Let "x" (Const 6) l1
l3 = Let "y" (Const 7) l2

v0 = runEval l1 []
v1 = runEval l1 [("x",2),("y",3)]
v2 = runEval l1 [("x",4),("y",5)]
v3 = runEval l2 [("y",5)]
v4 = runEval l3 []

-- -}
-- ----------------------------------------

