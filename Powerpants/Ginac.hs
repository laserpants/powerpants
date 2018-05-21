module Powerpants.Ginac
  ( Expr(..)
  , Powerpants.Ginac.abs
  , Powerpants.Ginac.div
  , Powerpants.Ginac.signum
  , Powerpants.Ginac.sqrt
  , add
  , diff
  , eval
  , factorial
  , mul
  , neg
  , num
  , pow
  , printEx
  , subs
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

makeForeign :: IO (Ptr GinacEx) -> Expr
makeForeign io = unsafeExpr (io >>= newForeignPtr ginac_ex_free_fun)

symbol :: GinacExPtr
{-# NOINLINE symbol #-}
symbol = unsafePerformIO (ginac_ex_new_x >>= newForeignPtr ginac_ex_free_fun)

add :: Expr -> Expr -> Expr
add ex_1 ex_2 = makeForeign (applyBinop ginac_add ex_1 ex_2)

mul :: Expr -> Expr -> Expr
mul ex_1 ex_2 = makeForeign (applyBinop ginac_mul ex_1 ex_2)

div :: Expr -> Expr -> Expr
div ex_1 ex_2 = makeForeign (applyBinop ginac_div ex_1 ex_2)

pow :: Expr -> Expr -> Expr
pow ex_1 ex_2 = makeForeign (applyBinop ginac_pow ex_1 ex_2)

neg :: Expr -> Expr
neg (Ex ptr) = makeForeign (withForeignPtr ptr ginac_neg)

num :: Int -> Expr
num = makeForeign . ginac_ex_new_int

abs :: Expr -> Expr
abs (Ex ptr) = makeForeign (withForeignPtr ptr ginac_abs)

signum :: Expr -> Expr
signum (Ex ptr) = makeForeign (withForeignPtr ptr ginac_signum)

diff :: Expr -> Expr
diff (Ex ptr) = makeForeign (withForeignPtr ptr ginac_diff)

factorial :: Int -> Expr
factorial = makeForeign . ginac_factorial

sqrt :: Expr -> Expr
sqrt (Ex ptr) = makeForeign (withForeignPtr ptr ginac_sqrt)

subs :: Expr -> Int -> Expr
subs (Ex ptr) = makeForeign . withForeignPtr ptr . ginac_subs

eval :: Expr -> Int -> Maybe Double
eval (Ex ptr) i = unsafePerformIO (withForeignPtr ptr stuff)
  where
    stuff ptr = do
      ex <- ginac_subs i ptr
      isNum <- ginac_is_numeric ex
      if isNum
          then fmap Just (ginac_ex_to_double ex)
          else pure Nothing

isNumeric :: Expr -> Bool
isNumeric (Ex ptr) = unsafePerformIO (withForeignPtr ptr ginac_is_numeric)

rational :: Rational -> Expr
rational r = fromInteger (numerator r) / fromInteger (denominator r)

printEx :: Expr -> IO ()
printEx (Ex ptr) = withForeignPtr ptr ginac_ex_print
