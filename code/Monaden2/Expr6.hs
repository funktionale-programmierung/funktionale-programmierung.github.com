{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Expr6 where

import           Control.Applicative ((<$>))
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State

import           System.IO


data Expr  = Const  Int
           | Var    Id
           | Assign Id    Expr
           | If     Expr  Expr Expr
           | While  Expr  Expr
           | Binary BinOp Expr Expr
           | Read
           | Write  String Expr
             deriving (Show)

data BinOp = Add | Sub | Mul | Div | Mod
           | Seq
             deriving (Eq, Show)

type Id    = String



type VarState = [(Id, Int)]

data ResVal a
           = Val { val :: a }
           | Exc { exc :: String }
             deriving (Show)

newtype Result a
           = Res { unRes :: VarState -> IO (ResVal a, VarState) }


instance Monad Result where
  return x       = Res $ \ st0 -> return (Val x, st0)
  (Res f1) >>= g = Res $ \ st0 ->
                   do (r1, st1) <- f1 st0
                      case r1 of
                        (Exc e) -> return (Exc e, st1)
                        (Val v) -> let Res f2 = g v in
                                   f2 st1

instance MonadError String Result where
  throwError s
      = Res $ \ st -> return (Exc s, st)

  -- not yet used in expression evaluation
  catchError (Res f) handler
      = Res $ \ st0 ->
        do (r1, st1) <- f st0
           case r1 of
             (Exc e) -> unRes (handler e) st1
             x       -> return (x, st1)

instance MonadState VarState Result where
  get       = Res $ \ st -> return (Val st, st)
  put new   = Res $ \ _  -> return (Val (), new)

instance MonadIO Result where
  liftIO a  = Res $ \ st ->
              do v <- a
                 return (Val v, st)

-- ----------------------------------------

eval :: Expr -> Result Int
eval (Const i)
    = return i

eval (Binary op l r)
    = do f <- lookupFtab op
         f (eval l) (eval r)

eval (Var ident)
    = do st <- get
         case lookup ident st of
           Nothing -> throwError
                      $ "undefined variable '"
                        ++ ident
                        ++ "' found"
           Just v  -> return v

eval (Assign ident e1)
    = do v1 <- eval e1
         st <- get
         put   (setVar ident v1 st)
         -- or shorter: modify (setVar ident v1)
         return v1
    where
      setVar i v s = (i, v) : filter ((/= i) . fst) s

eval (If c t e)
    = do v <- eval c
         if v /= 0
           then eval t
           else eval e

eval st@(While c b)
    = do v <- eval c
         if v /= 0
           then do eval b
                   eval st
           else return v

eval Read
    = readInt

eval (Write msg e)
    = do v <- eval e
         writeInt msg v
         return v

-- ----------------------------------------

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
       , (Seq, liftM2 (\ x y -> y))
       ]

div' :: Int -> Int -> Result Int
div' x y
  | y == 0    = throwError
                "division by zero"
  | otherwise = return (x `div` y)

-- ----------------------------------------
-- system functions

readInt :: Result Int
readInt
    = liftIO $
      do
      hPutStr stdout "give a number: "
      hFlush  stdout
      line <- hGetLine stdin
      return ((read line)::Int)

writeInt :: Show a => String -> a -> Result ()
writeInt m v
    = liftIO $
      hPutStrLn stdout (m ++ show v)

-- ----------------------------------------
-- evaluate an expression within a given state

runEval :: Expr -> VarState -> IO (ResVal Int, VarState)
runEval e st0
    = let (Res f) = eval e in
      f st0

eval'   :: Expr -> IO (ResVal Int)
eval' e =  fst <$> runEval e []

exec'   :: Expr -> IO VarState
exec' e =  snd <$> runEval e []

-- ----------------------------------------
-- sample expressions

e1 = Binary Mul (Binary Add (Const 2)
                            (Const 4)
                )
                (Const 7)
e2 = Binary Div (Const 1) (Const 0)
e3 = Binary Mod (Const 1) (Const 0)
e4 = Var "x"
e5 = Binary Mul (Binary Add e4
                            (Const 1)
                ) e4

v1 = eval' e1
v2 = eval' e2
v3 = eval' e3

v4  = runEval  e4 [("x", 42)]
v5  = runEval  e5 [("x",  6)]

-- swap x an y

p0 = foldr1 (Binary Seq)
     [ Assign "t" (Var "x")
     , Assign "x" (Var "y")
     , Assign "y" (Var "t")
     ]

-- count to 100 with trace
p2 = While
       (Binary Sub x (Const 100))
       (Write "x = "
          (Assign "x" (Binary Add x (Const 1))))
    where
      x = Var "x"

-- read 2 numbers and display product
p1 = Write "Result is: "
     (Binary Mul Read Read)


t0 = runEval p0 [("x", 42), ("y",23)]
t1 = runEval p1 []
t2 = runEval p2 [("x", 1)]

-- ----------------------------------------

