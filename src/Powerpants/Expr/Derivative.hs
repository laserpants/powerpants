{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Powerpants.Expr.Derivative where

import Algebra.Ring
import Powerpants.Expr
import NumericPrelude

ddx :: Algebra.Ring.C a => Expr a -> Expr a
ddx X            = Num 1
ddx (Num _)      = Num 0
ddx (Add xs)     = Add (fmap ddx xs)
ddx (Mul [])     = Num 0
ddx (Mul [x])    = ddx x
ddx (Mul (x:xs)) = Add [Mul [ddx x, Mul xs], Mul [x, ddx (Mul xs)]]
ddx (Pow a n)    = Mul [Num (fromIntegral n), Pow a (n - 1), ddx a]
