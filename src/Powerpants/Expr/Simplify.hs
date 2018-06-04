{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Powerpants.Expr.Simplify where

import Algebra.Ring
import NumericPrelude
import Powerpants.Expr

--
-- Mul [x, y, Num m, z, Num n, ...]   ==>   Mul [x, y, z, Num (m * n), ...]
-- Add [x, y, Num m, z, Num n, ...]   ==>   Add [x, y, z, Num (m + n), ...]
-- Pow (Num x) n                      ==>   Num (x^n)
--

-- | Combine multiple constants that occur below an addition or multiplication
--   node by adding or multiplying them into a single constant.
foldnums :: (Algebra.Ring.C a, Eq a, Ord a)
         => Expr a
         -> Expr a
foldnums (Mul xs) =
    let (nums, rest) = collectNums xs
      in case product nums of
        0 -> Num 0
        1 -> Mul rest
        n -> Mul (Num n:rest)
foldnums (Add xs) =
    let (nums, rest) = collectNums xs
      in case sum nums of
        0 -> Add rest
        n -> Add (Num n:rest)
foldnums (Pow (Num 0) n)      = Num 0
foldnums (Pow a 0)            = Num 1
foldnums (Pow a 1)            = a
foldnums expr@(Pow (Num a) n) = let m = a^n in if m < 5000 then Num m else expr
foldnums (Div a (Num 1))      = a
foldnums expr                 = expr

-- | Replace empty addition and multiplication nodes with the identity element
--   of the operation (i.e., 0 for addition and 1 for multiplication), and
--   substitute the only element for lists with one single expression. After
--   this step, any 'Add' or 'Mul' node will have at least two children.
collapse :: (Algebra.Ring.C a) => Expr a -> Expr a
collapse (Add [ ]) = Num 0
collapse (Add [x]) = x
collapse (Mul [ ]) = Num 1
collapse (Mul [x]) = x
collapse expr      = expr

--
-- Mul [a, b, Mul [x, y, ...], ...]   ==>   Mul [a, b, x, y, ...]
-- Add [a, b, Add [x, y, ...], ...]   ==>   Add [a, b, x, y, ...]
--
flatten :: Expr a -> Expr a
flatten expr =
    case expr of
      Mul xs -> Mul (level maybeMul xs)
      Add xs -> Add (level maybeAdd xs)
      _      -> expr
  where
    level :: (a -> Maybe [a]) -> [a] -> [a]
    level fn = rec [] where
        rec exs' [] = exs'
        rec exs' (ex:exs) = rec res exs where
            res = case fn ex of
              Just xs -> xs ++ exs'
              Nothing -> ex:exs'
