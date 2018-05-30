{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Powerpants.Polynomial where

import Algebra.Additive
import Algebra.IntegralDomain
import Algebra.Ring
import Control.Applicative      ( pure )
import NumericPrelude
import NumericPrelude.Base

newtype Polynomial a = Px { coefficients :: [a] } deriving (Show, Eq)

trim :: (Algebra.IntegralDomain.C a, Eq a)
     => Polynomial a
     -> Polynomial a
trim = Px . canon . reverse . dropWhile (== 0). reverse . coefficients where
    canon [] = [zero]
    canon xs = xs

instance (Algebra.IntegralDomain.C a, Eq a) => Algebra.Additive.C (Polynomial a)
  where
    (+)    = add
    negate = neg
    zero   = Px [zero]

add :: (Algebra.IntegralDomain.C a, Eq a)
    => Polynomial a
    -> Polynomial a
    -> Polynomial a
add (Px xs) (Px ys) = trim (Px (rec xs ys)) where
    rec xs [] = xs
    rec [] ys = ys
    rec (x:xs) (y:ys) = x + y : rec xs ys

neg :: (Algebra.IntegralDomain.C a, Eq a)
    => Polynomial a
    -> Polynomial a
neg (Px xs) = Px (fmap negate xs)

instance (Algebra.IntegralDomain.C a, Eq a) => Algebra.Ring.C (Polynomial a)
  where
    (*) = mul
    fromInteger = constant . fromIntegral

mul :: (Algebra.IntegralDomain.C a, Eq a)
    => Polynomial a
    -> Polynomial a
    -> Polynomial a
mul (Px xs) (Px ys) = trim (foldr1 add (rec xs ys)) where
    rec [] _      = [0]
    rec (x:xs) ys = Px (fmap (*x) ys) : rec xs (0 : ys)

constant :: (Algebra.IntegralDomain.C a, Eq a) => a -> Polynomial a
constant = Px . pure

x :: (Algebra.IntegralDomain.C a, Eq a) => Polynomial a
x = Px [0, 1]

derivative :: (Algebra.IntegralDomain.C a, Eq a, Enum a)
           => Polynomial a
           -> Polynomial a
derivative (Px (_:xs)) = trim (Px (zipWith (*) xs [1..]))

eval :: (Algebra.Ring.C a) => Polynomial a -> a -> a
eval (Px xs) c = sum (zipWith (*) xs [c^n | n <- [0..]])
