module Powerpants where

import Ginac.FFI

data Expr = Expr Ginac | X

data GF = GF Expr

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
