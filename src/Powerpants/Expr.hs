module Powerpants.Expr 
  ( Expr(..)
  , newEx
  ) where

import Foreign
import Foreign.C.String
import Math.Ginac.FFI
import Powerpants

data Expr = 
      X 
    | Num  !Int 
    | Neg  !Expr 
    | Abs  !Expr 
    | Sqrt !Expr 
    | Sign !Expr 
    | Add  !Expr !Expr
    | Sub  !Expr !Expr
    | Mul  !Expr !Expr
    | Div  !Expr !Expr
    | Pow  !Expr !Expr
  deriving (Show, Eq)

newEx :: Expr -> IO FunEx
newEx expr = do
    symbol <- withCString "x" ginac_symbol_new
    ex <- constr expr symbol 
    newFunEx ex symbol

constr :: Expr -> Ptr GinacSymbol -> IO (Ptr GinacEx)
constr X         s = ginac_ex_new_from_basic s
constr (Num a)   _ = ginac_ex_new_from_int a
constr (Add a b) s = fn2 ginac_ex_add a b s
constr (Sub a b) s = fn2 ginac_ex_sub a b s
constr (Mul a b) s = fn2 ginac_ex_mul a b s
constr (Div a b) s = fn2 ginac_ex_div a b s
constr (Pow a b) s = fn2 ginac_ex_pow a b s
constr (Neg a)   s = fn ginac_ex_neg a s
constr (Abs a)   s = fn ginac_ex_abs a s
constr (Sqrt a)  s = fn ginac_ex_sqrt a s
constr (Sign a)  s = fn ginac_ex_signum a s

fn :: (Ptr GinacEx -> IO (Ptr GinacEx))
   -> Expr 
   -> Ptr GinacSymbol 
   -> IO (Ptr GinacEx)
fn f a s = do
    x <- constr a s
    res <- f x
    ginac_ex_free x
    pure res

fn2 :: (Ptr GinacEx -> Ptr GinacEx -> IO (Ptr GinacEx)) 
      -> Expr 
      -> Expr 
      -> Ptr GinacSymbol 
      -> IO (Ptr GinacEx)
fn2 op a b s = do
    lhs <- constr a s
    rhs <- constr b s
    res <- op lhs rhs
    ginac_ex_free lhs
    ginac_ex_free rhs
    pure res
