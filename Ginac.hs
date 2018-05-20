module Ginac where

import Foreign
import Ginac.FFI

newtype Expr = Expr Ginac

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

symbolX :: Ginac
symbolX = undefined
