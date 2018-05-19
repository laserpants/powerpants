module Powerpants where

import Ginac.FFI

newtype Expr = Expr Ginac

newtype GF = GF Expr

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

--instance Num GF where
--    x + y        = undefined
--    x * y        = undefined
--    negate       = undefined
--    abs    x     = undefined
--    signum x     = undefined
--    fromInteger  = undefined

x :: Expr
x = undefined
