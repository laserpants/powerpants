{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Powerpants.Expr.Derivative where

import Algebra.Field
import Algebra.RealRing
import Algebra.Ring
import Algebra.ToInteger
import NumericPrelude
import Powerpants.Expr
import Powerpants.Expr.Simplify

ddx :: Algebra.Ring.C a => Expr a -> Expr a
ddx X            = Num 1
ddx (Num _)      = Num 0
ddx (Add xs)     = Add (fmap ddx xs)
ddx (Mul [])     = Num 0
ddx (Mul [x])    = ddx x
ddx (Mul (x:xs)) = Add [Mul [ddx x, Mul xs], Mul [x, ddx (Mul xs)]]
ddx (Pow a n)    = Mul [Num (fromIntegral n), Pow a (n - 1), ddx a]

derivatives :: (Algebra.ToInteger.C a) => Expr a -> [Expr a]
derivatives = rec 1 where
  rec :: (Algebra.ToInteger.C a) => Int -> Expr a -> [Expr a]
  rec n expr = expr : rec (n + 1) (simplified' (ddx expr `divexpr` Num (fromIntegral (n + 1))))

expansion :: (Algebra.ToInteger.C a, Ord a)
          => Expr a
          -> [Double]
          -- -> [b]
expansion = rec 1
  where
    rec :: (Algebra.ToInteger.C a, Ord a)
        => Int
        -> Expr a
        -> [Double]
        -- -> [b]
    rec n expr = eval 0 expr : rec next (simplified'' expr') where
        expr' = ddx expr `divexpr` Num (fromIntegral next)
        next  = n + 1
