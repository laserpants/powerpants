module Ginac.FFI where

import Foreign

data GinacExpr

type Ginac = ForeignPtr GinacExpr

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
