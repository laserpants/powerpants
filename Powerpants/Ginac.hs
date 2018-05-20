module Powerpants.Ginac
  ( Expr(..)
  , Powerpants.Ginac.abs
  , Powerpants.Ginac.div
  , Powerpants.Ginac.signum
  , add
  , mul
  , neg
  , num
  , printEx
  , symbol
  ) where

import Data.Ratio
import Foreign
import Powerpants.Ginac.FFI
import System.IO.Unsafe

newtype Expr = Ex GinacExPtr

instance Num Expr where
    x + y        = add x y
    x * y        = mul x y
    negate       = neg
    abs          = Powerpants.Ginac.abs
    signum       = Powerpants.Ginac.signum
    fromInteger  = num . fromIntegral

instance Fractional Expr where
    x / y        = Powerpants.Ginac.div x y
    fromRational = rational

type Binop = Ptr GinacEx -> Ptr GinacEx -> IO (Ptr GinacEx)

applyBinop :: Binop -> Expr -> Expr -> IO (Ptr GinacEx)
applyBinop op (Ex ex_1) (Ex ex_2) =
  withForeignPtr ex_1 $ \ptr_1 ->
    withForeignPtr ex_2 $ \ptr_2 ->
      op ptr_1 ptr_2

unsafeExpr :: IO GinacExPtr -> Expr
unsafeExpr = Ex . unsafePerformIO

symbol :: GinacExPtr
{-# NOINLINE symbol #-}
symbol = unsafePerformIO (ginac_ex_new_x >>= newForeignPtr ginac_ex_free_fun)

add :: Expr -> Expr -> Expr
add ex_1 ex_2 = unsafeExpr expr where
  expr = applyBinop ginac_add ex_1 ex_2 >>= newForeignPtr ginac_ex_free_fun

mul :: Expr -> Expr -> Expr
mul = undefined

div :: Expr -> Expr -> Expr
div = undefined

neg :: Expr -> Expr
neg = undefined

num :: Int -> Expr
num i = unsafeExpr (ginac_ex_new_int i >>= newForeignPtr ginac_ex_free_fun)

abs :: Expr -> Expr
abs (Ex ptr) = unsafeExpr expr where
  expr = withForeignPtr ptr ginac_abs >>= newForeignPtr ginac_ex_free_fun

signum :: Expr -> Expr
signum = undefined

rational :: Rational -> Expr
rational r = fromInteger (numerator r) / fromInteger (denominator r)

printEx :: Expr -> IO ()
printEx (Ex ptr) = withForeignPtr ptr ginac_ex_print
