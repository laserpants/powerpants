module Ginac where

import Foreign
import Ginac.FFI
import System.IO.Unsafe

newtype Expr = Expr GinacExPtr

instance Num Expr where
    x + y        = undefined
    x * y        = undefined
    negate       = undefined
    abs    x     = undefined
    signum x     = undefined
    fromInteger  = undefined

instance Fractional Expr where
    x / y        = undefined
    fromRational = undefined

symbol :: GinacExPtr
{-# NOINLINE symbol #-}
symbol = unsafePerformIO (ginac_ex_new_x >>= newForeignPtr ginac_ex_free_fun)
