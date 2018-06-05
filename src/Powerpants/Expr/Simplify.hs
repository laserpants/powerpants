{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Powerpants.Expr.Simplify where

import Algebra.Ring
import Data.Functor        ( (<$>) )
import Data.List           ( sort, insert )
import NumericPrelude
import Powerpants.Expr

-- | Partition a list of 'Expr's into two lists; one with all constants (i.e.,
--   values wrapped in 'Num' constructors), and another with the remaining
--   expressions.
collectNums :: [Expr a] -> ([a], [Expr a])
collectNums = rec ([], []) where
    rec p [] = p
    rec (nums, xs') (x:xs) = rec p' xs where
        p' = case x of
               Num n -> (n:nums, xs')
               _     -> (nums, x:xs')

-- | Combine multiple constants under an addition or multiplication node by
--   adding or multiplying them into a single 'Num' value.
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

-- | Replace empty addition and multiplication nodes with zero or one (i.e.,
--   the operation's identity), and /pull out/ the expression from lists with
--   only one element. After this step, any 'Add' or 'Mul' node will have at
--   least two children.
collapse :: (Algebra.Ring.C a) => Expr a -> Expr a
collapse (Add [ ]) = Num 0
collapse (Add [x]) = x
collapse (Mul [ ]) = Num 1
collapse (Mul [x]) = x
collapse expr      = expr

-- | Flatten (or /level/) nested addition and multiplication nodes.
flatten :: Expr a -> Expr a
flatten expr =
    case expr of
      Mul xs -> Mul (level unwrapMul xs)
      Add xs -> Add (level unwrapAdd xs)
      _      -> expr
  where
    level :: (a -> Maybe [a]) -> [a] -> [a]
    level fn = rec [] where
        rec exs' [] = exs'
        rec exs' (ex:exs) = rec res exs where
            res = case fn ex of
              Just xs -> xs ++ exs'
              Nothing -> ex:exs'

-- | Transform the syntax tree so that a division node cannot be the immediate
--   child of a division node or a multiplication node.
divnode :: Expr a -> Expr a
divnode (Div (Div a b) c) = Div a (Mul [b, c])
divnode (Div a (Div b c)) = Div (Mul [a, c]) b
divnode (Mul xs) =
    case rec [] xs of
      (lhs, Div a b:rhs) -> let xs' = lhs ++ a:rhs in Div (divnode (Mul xs')) b
      _                  -> Mul xs
  where
    rec ys []            = (reverse ys, [])
    rec ys xs@(Div {}:_) = (reverse ys, xs)
    rec ys (x:xs)        = rec (x:ys) xs
divnode expr = expr

ordered :: Ord a => Expr a -> Expr a
ordered (Add xs)  = Add (ordered <$> sort xs)
ordered (Mul xs)  = Mul (ordered <$> sort xs)
ordered (Div a b) = Div (ordered a) (ordered b)
ordered (Pow a n) = Pow (ordered a) n
ordered expr      = expr

groupSimilar :: (Ord a, Eq a) => [Expr a] -> [(Expr a, Int)]
groupSimilar = foldr fn [] where
    fn expr al =
        case lookup expr al of
          Just n  -> insert (expr, succ n) (filter ((/= expr) . fst) al)
          Nothing -> insert (expr, 1) al

-- canonical :: Expr a -> Expr a
-- canonical (Add xs)  = undefined
-- canonical (Mul xs)  = undefined
-- canonical (Div a b) = undefined
-- canonical (Pow a n) = undefined
-- canonical (Num n)   = undefined
-- canonical X         = X
