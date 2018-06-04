{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Powerpants.Expr where

import Algebra.Field
import Algebra.Ring
import NumericPrelude

data Expr a
  = X
  | Num !a
  | Add ![Expr a]
  | Mul ![Expr a]
  | Div !(Expr a) !(Expr a)
  | Pow !(Expr a) !Integer
  deriving (Show, Eq, Ord)

neg :: Algebra.Ring.C a => Expr a -> Expr a
neg (Num n) = Num (negate n)
neg ex = Mul [Num (-1), ex]

sub :: Algebra.Ring.C a => Expr a -> Expr a-> Expr a
sub a b = Add [a, neg b]

eval :: Algebra.Field.C a => a -> Expr a -> a
eval x X         = x
eval _ (Num n)   = n
eval x (Add xs)  = sum (fmap (eval x) xs)
eval x (Mul xs)  = product (fmap (eval x) xs)
eval x (Div a b) = eval x a / eval x b
eval x (Pow a n) = eval x a^n

isX, isAdd, isMul, isDiv, isPow :: Expr a -> Bool

isX X = True
isX _ = False

isNum (Num _) = True
isNum _ = False

isAdd (Add _) = True
isAdd _ = False

isMul (Mul _) = True
isMul _ = False

isDiv (Div _ _) = True
isDiv _ = False

isPow (Pow _ _) = True
isPow _ = False

maybeNum :: Expr a -> Maybe a
maybeNum (Num n) = Just n
maybeNum _ = Nothing

maybeAdd :: Expr a -> Maybe [Expr a]
maybeAdd (Add xs) = Just xs
maybeAdd _ = Nothing

maybeMul :: Expr a -> Maybe [Expr a]
maybeMul (Mul xs) = Just xs
maybeMul _ = Nothing
