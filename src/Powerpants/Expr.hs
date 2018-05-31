{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Powerpants.Expr where

import Algebra.Additive
import Algebra.IntegralDomain
import Algebra.Ring
import NumericPrelude
import NumericPrelude.Base
import Powerpants.Polynomial

type P = Polynomial

data Expr a
  = Poly !(P a)        -- p(x)
  | Frac !(P a) !(P a) -- p(x)/q(x)
  | Recp !(P a)        -- 1/p(x)
  deriving (Show, Eq)

frac :: (Algebra.IntegralDomain.C a, Eq a, Enum a)
     => Polynomial a
     -> Polynomial a
     -> Expr a
frac gx hx
  |  1 == gx  = Recp hx
  | -1 == gx  = Recp (negate hx)
  | otherwise = Frac gx hx

ddx :: (Algebra.IntegralDomain.C a, Eq a, Enum a) => Expr a -> Expr a
ddx (Poly px) = Poly (derivative px)
ddx (Recp gx) = frac (negate (derivative gx)) (gx^2)
