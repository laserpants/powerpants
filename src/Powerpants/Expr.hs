{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Powerpants.Expr
  where
--  ( Expr(..)
--  , neg
--  , sub
--  , eval
--  -- * Predicates
--  , isX
--  , isNum
--  , isAdd
--  , isMul
--  , isPow
--  -- * Unwrappers
--  , unwrapNum
--  , unwrapAdd
--  , unwrapMul
--  -- * Canonical form
--  ) where

import Algebra.Field
import Algebra.Ring
import NumericPrelude

-- | Data type representation of algebraic expressions in one variable.
data Expr a
  = Num !a
  -- ^ A numeric value
  | X
  -- ^ The variable /x/
  | Add ![Expr a]
  -- ^ Addition node
  | Mul ![Expr a]
  -- ^ Multiplication node
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
--
-- >>> eval 5 (Add [X, Num 3])
-- 8.0
eval :: Algebra.Field.C a => a -> Expr a -> a
eval x X         = x
eval _ (Num n)   = n
eval x (Add xs)  = sum (fmap (eval x) xs)
eval x (Mul xs)  = product (fmap (eval x) xs)
eval x (Pow a n) = eval x a^n

isX, isAdd, isMul, isPow :: Expr a -> Bool

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

--normalized :: Algebra.Ring.C a => Expr a -> Expr a
--normalized (Add xs) = Add (fmap std xs) where
--    std X         = Mul [Num 1, X]
--    std (Add xs)  = Mul [Num 1, Add xs]
--    std (Mul xs)  = Mul xs
--    std (Div a b) = undefined
--    std (Pow a n) = Mul [Num 1, Pow a n]
--    std subex     = subex
--normalized (Mul xs)  = undefined
--normalized (Div a b) = Div (normalized a) (normalized b)
--normalized (Pow a n) = Pow (normalized a) n
--normalized expr      = expr

expr1 :: Algebra.Ring.C a => Expr a
expr1 = Add
  [ Mul [Num 5, X]
  , Pow X (-10)
  , Pow X 3
  --, Div (Mul [Num 2, Pow X 2]) (Num 3)
  , Num 7 ]
