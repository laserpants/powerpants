{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Powerpants.Expr where

import Algebra.Field
import Algebra.Ring
import NumericPrelude

-- | Data type representation of algebraic expressions.
data Expr a
  = X
  | Num !a
  | Add ![Expr a]
  | Mul ![Expr a]
  | Div !(Expr a) !(Expr a)
  | Pow !(Expr a) !Integer
  deriving (Show, Eq, Ord)

-- | Negation: Return an 'Expr' equal to the additive inverse of the input.
neg :: Algebra.Ring.C a => Expr a -> Expr a
neg ex = Mul [Num (-1), ex]

-- | Subtraction is just the negative of the second 'Expr' added to the first.
sub :: Algebra.Ring.C a => Expr a -> Expr a-> Expr a
sub a b = Add [a, neg b]

-- | Evaluate an expression at the point /x/.
eval :: Algebra.Field.C a => a -> Expr a -> a
eval x X         = x
eval _ (Num n)   = n
eval x (Add xs)  = sum (fmap (eval x) xs)
eval x (Mul xs)  = product (fmap (eval x) xs)
eval x (Div a b) = eval x a / eval x b
eval x (Pow a n) = eval x a^n

isX, isAdd, isMul, isDiv, isPow :: Expr a -> Bool

-- | Predicate to test if a value matches the 'X' constructor.
isX X = True
isX _ = False

-- | Predicate to test if a value matches the 'Num' constructor.
isNum (Num _) = True
isNum _ = False

-- | Predicate to test if a value matches the 'Add' constructor.
isAdd (Add _) = True
isAdd _ = False

-- | Predicate to test if a value matches the 'Mul' constructor.
isMul (Mul _) = True
isMul _ = False

-- | Predicate to test if a value matches the 'Div' constructor.
isDiv (Div _ _) = True
isDiv _ = False

-- | Predicate to test if a value matches the 'Pow' constructor.
isPow (Pow _ _) = True
isPow _ = False

-- | Unwrap and return the value of an expression if it matches the 'Num'
--   constructor.
unwrapNum :: Expr a -> Maybe a
unwrapNum (Num n) = Just n
unwrapNum _ = Nothing

-- | Unwrap and return the value of an expression if it matches the 'Add'
--   constructor.
unwrapAdd :: Expr a -> Maybe [Expr a]
unwrapAdd (Add xs) = Just xs
unwrapAdd _ = Nothing

-- | Unwrap and return the value of an expression if it matches the 'Mul'
--   constructor.
unwrapMul :: Expr a -> Maybe [Expr a]
unwrapMul (Mul xs) = Just xs
unwrapMul _ = Nothing
