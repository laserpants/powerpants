{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Powerpants.Polynomial where

import Algebra.Additive
import Algebra.IntegralDomain
import Algebra.Ring
import Control.Applicative      ( pure )
import NumericPrelude
import NumericPrelude.Base

newtype Polynomial a = Px { coefficients :: [a] }
  deriving (Show, Eq)

trim :: (Algebra.IntegralDomain.C a, Eq a) => [a] -> [a]
trim = canon . reverse . dropWhile (== 0) . reverse where
    canon [] = [zero]
    canon xs = xs

px :: (Algebra.IntegralDomain.C a, Eq a) => [a] -> Polynomial a
px = Px . trim

instance (Algebra.IntegralDomain.C a, Eq a) => Algebra.Additive.C (Polynomial a)
  where
    (+)    = add
    negate = neg
    zero   = Px [zero]

add :: (Algebra.IntegralDomain.C a, Eq a)
    => Polynomial a
    -> Polynomial a
    -> Polynomial a
add (Px xs) (Px ys) = px (xs + ys)

neg :: (Algebra.IntegralDomain.C a, Eq a) => Polynomial a -> Polynomial a
neg (Px xs) = Px (fmap negate xs)

instance (Algebra.IntegralDomain.C a, Eq a) => Algebra.Ring.C (Polynomial a)
  where
    (*) = mul
    fromInteger = constant . fromIntegral

mul :: (Algebra.IntegralDomain.C a, Eq a)
    => Polynomial a
    -> Polynomial a
    -> Polynomial a
mul (Px xs) (Px ys) = px (rec xs ys) where
    rec [] ys = [0]
    rec xs [] = [0]
    rec (x:xs) (y:ys) = x * y : x.* ys + y.* xs + (0 : rec xs ys)
    (.*) x = map (x*)

constant :: (Algebra.IntegralDomain.C a, Eq a) => a -> Polynomial a
constant = Px . pure

x :: (Algebra.IntegralDomain.C a, Eq a) => Polynomial a
x = Px [0, 1]

derivative :: (Algebra.IntegralDomain.C a, Eq a, Enum a)
           => Polynomial a
           -> Polynomial a
derivative (Px (_:xs)) = px (zipWith (*) xs [1..])

eval :: (Algebra.Ring.C a) => Polynomial a -> a -> a
eval (Px xs) c = sum (zipWith (*) xs [c^n | n <- [0..]])
