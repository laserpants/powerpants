module Ginac 
  ( Expr(..)
  , printEx
  , symbol
  ) where

import Foreign
import Ginac.FFI
import System.IO.Unsafe

newtype Expr = Ex GinacExPtr

instance Num Expr where
    x + y        = add x y
    x * y        = undefined
    negate       = undefined
    abs    x     = undefined
    signum x     = undefined
    fromInteger  = num . fromIntegral

instance Fractional Expr where
    x / y        = undefined
    fromRational = undefined

type Binop = Ptr GinacEx -> Ptr GinacEx -> IO (Ptr GinacEx)

applyBinop :: Binop -> GinacExPtr -> GinacExPtr -> IO (Ptr GinacEx)
applyBinop op ex_1 ex_2 = 
  withForeignPtr ex_1 $ \ptr_1 -> 
    withForeignPtr ex_2 $ \ptr_2 ->
      op ptr_1 ptr_2

unsafeExpr :: IO GinacExPtr -> Expr
unsafeExpr = Ex . unsafePerformIO

symbol :: GinacExPtr
{-# NOINLINE symbol #-}
symbol = unsafePerformIO (ginac_ex_new_x >>= newForeignPtr ginac_ex_free_fun)

add :: Expr -> Expr -> Expr
add (Ex ex_1) (Ex ex_2) = unsafeExpr ptr where
  ptr = applyBinop ginac_add ex_1 ex_2 >>= newForeignPtr ginac_ex_free_fun

num :: Int -> Expr
num i = unsafeExpr (ginac_ex_new_int i >>= newForeignPtr ginac_ex_free_fun)

printEx :: Expr -> IO ()
printEx (Ex ptr) = withForeignPtr ptr ginac_ex_print
