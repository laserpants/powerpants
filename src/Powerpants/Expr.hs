{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Powerpants.Expr where

import Algebra.Field
import Algebra.Ring
import NumericPrelude

-- | Data type representation of algebraic expressions in one variable.
data Expr a
  = X
  -- ^ The variable /x/
  | Num !a
  -- ^ A numeric value
  | Add ![Expr a]
  -- ^ Addition node
  | Mul ![Expr a]
  -- ^ Multiplication node
  | Div !(Expr a) !(Expr a)
  -- ^ The ratio of two expressions
  | Pow !(Expr a) !Integer
  -- ^ A value raised to an integer power
  deriving (Show, Eq, Ord)

-- | Return an 'Expr' equal to the additive inverse of the input.
neg :: Algebra.Ring.C a => Expr a -> Expr a
neg ex = Mul [Num (-1), ex]

-- | Subtraction implemented as the negative of the r.h.s. 'Expr' added to the 
--   l.h.s. value.
--
-- > sub a b = Add [a, neg b]
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

-- | Return the value contained by the expression, if it is a constant.
unwrapNum :: Expr a -> Maybe a
unwrapNum (Num n) = Just n
unwrapNum _ = Nothing

-- | Return the list of child expressions, if the value represents an 
--   addition node.
unwrapAdd :: Expr a -> Maybe [Expr a]
unwrapAdd (Add xs) = Just xs
unwrapAdd _ = Nothing

-- | Return the list of child expressions, if the value represents an 
--   multiplication node.
unwrapMul :: Expr a -> Maybe [Expr a]
unwrapMul (Mul xs) = Just xs
unwrapMul _ = Nothing
