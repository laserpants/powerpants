{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Powerpants.Expr where

import Algebra.Additive
import Algebra.Field
import Algebra.IntegralDomain
import Algebra.Ring
import Algebra.ZeroTestable
import MathObj.Polynomial
import MathObj.Polynomial.Core   ( differentiate )
import Number.Ratio
import NumericPrelude
import NumericPrelude.Base

data Expr a
  = Px   !(MathObj.Polynomial.T a)
  | Frac !(MathObj.Polynomial.T a)
         !(MathObj.Polynomial.T a)
--  | Pow  !(MathObj.Polynomial.T a) 
--         !Integer
  deriving (Show, Eq)

frac :: (Algebra.Field.C a, Algebra.ZeroTestable.C a, Eq a)
     => MathObj.Polynomial.T a
     -> MathObj.Polynomial.T a
     -> Expr a
frac 0 q = Px 0
frac p 1 = Px p
frac p q = Frac a b where a :% b = p % q

ddx :: (Algebra.Field.C a, Algebra.ZeroTestable.C a, Eq a)
    => Expr a
    -> Expr a
ddx (Px px) = Px (diff px)
ddx (Frac px qx) =
    case coeffs px of
      [n] -> frac (MathObj.Polynomial.const n * negate (diff qx)) (qx^2)
      _   -> frac (diff px * qx - diff qx * px) (qx^2)
-- ddx (Pow px n) = undefined

diff :: (Algebra.Field.C a, Algebra.ZeroTestable.C a, Eq a)
     => MathObj.Polynomial.T a
     -> MathObj.Polynomial.T a
diff = fromCoeffs . differentiate . coeffs

eval :: (Algebra.Field.C a, Algebra.ZeroTestable.C a, Eq a) => Expr a -> a -> a
eval (Px px)      x = evaluate px x
eval (Frac px qx) x = evaluate px x / evaluate qx x
-- eval (Pow px n)   x = (evaluate px x)^n

xxx :: (Algebra.Field.C a, Algebra.ZeroTestable.C a, Eq a) => Expr a -> [a]
xxx = rec where rec ex = eval ex 0 : rec (ddx ex)
