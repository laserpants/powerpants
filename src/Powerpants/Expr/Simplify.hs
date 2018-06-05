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

-- | Combine constants under an addition or multiplication node by adding or
--   multiplying them into a single 'Num' value. Repeated powers are eliminated
--   and powers of constants are evaluated and replaced with the result, if it
--   is less than 5000. The function calls itself recursively on child nodes.
folded :: (Algebra.Ring.C a, Eq a, Ord a)
       => Expr a
       -> Expr a
folded (Mul xs) =
    let (nums, rest) = collectNums xs
      in case product nums of
        0 -> Num 0
        1 -> Mul (folded <$> rest)
        n -> Mul (Num n:(folded <$> rest))
folded (Add xs) =
    let (nums, rest) = collectNums xs
      in case sum nums of
        0 -> Add (folded <$> rest)
        n -> Add (Num n:(folded <$> rest))
folded pow@(Pow (Num 0) 0) = pow -- To avoid having to think about 0^0
folded (Pow (Num 0) n)     = Num 0
folded (Pow a 0)           = Num 1
folded (Pow a 1)           = folded a
folded pow@(Pow (Num a) n) = let m = a^n in if m < 5000 then Num m else pow
folded (Pow (Pow a m) n)   = Pow (folded a) (m*n)
folded (Div a (Num 1))     = folded a
folded (Div a b)           = Div (folded a) (folded b)
folded expr                = expr

-- | Replace empty addition and multiplication nodes with zero or one (i.e.,
--   the operation's identity), and /pull out/ the expression from lists with
--   only one element. After this step, any 'Add' or 'Mul' node will have at
--   least two children.
collapsed :: (Algebra.Ring.C a) => Expr a -> Expr a
collapsed (Add [ ]) = Num 0
collapsed (Add [x]) = x
collapsed (Mul [ ]) = Num 1
collapsed (Mul [x]) = x
collapsed expr      = expr

-- | Flatten (or /level/) nested addition and multiplication nodes.
flattened :: Expr a -> Expr a
flattened expr =
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
    rec ys []             = (reverse ys, [])
    rec ys xs@(Div {}:_)  = (reverse ys, xs)
    rec ys (x:xs)         = rec (x:ys) xs
divnode expr = expr

-- | Return a /canonical/ representation of the node tree, ordered by the
--   derived 'Ord' instance.
ordered :: Ord a => Expr a -> Expr a
ordered (Add xs)  = Add (ordered <$> sort xs)
ordered (Mul xs)  = Mul (ordered <$> sort xs)
ordered (Div a b) = Div (ordered a) (ordered b)
ordered (Pow a n) = Pow (ordered a) n
ordered expr      = expr

groupSimilar :: (Ord a, Eq a) => [Expr a] -> [(Expr a, Int)]
groupSimilar = foldr fn [] where
    fn (Num n) al = (Num n, 1):al  -- Leave constants to be folded later
    fn expr al =
        case lookup expr al of
          Just n  -> insert (expr, succ n) (filter ((/= expr) . fst) al)
          Nothing -> insert (expr, 1) al

combineUsing :: (Algebra.Ring.C a, Ord a) => ((Expr a, Int) -> Expr a) -> [Expr a] -> [Expr a]
combineUsing fn xs = fmap (combined . fn) (groupSimilar xs)

-- | Collect and combine like expressions.
combined :: (Algebra.Ring.C a, Ord a, Eq a) => Expr a -> Expr a
combined (Mul xs) = Mul (combineUsing fn xs) where
    fn (expr, 0)        = Num 1
    fn (expr, 1)        = expr
    fn (Pow a n, count) = Pow a (n * fromIntegral count)
    fn (expr, count)    = Pow expr (fromIntegral count)
combined (Add xs) = Add (combineUsing fn xs) where
    fn (expr, 0)        = Num 0
    fn (expr, 1)        = expr
    fn (expr, count)    = Mul [Num (fromIntegral count), expr]
combined expr = expr

-- combined (Mul [Pow X 3, Pow X 5, X, X])  <-->  x^3 * x^5 * x * x

-- combined (Add [Pow X 3, Pow X 5, X, X])  <-->  x^3 + x^5 + x + x

-- combined (Add [Mul [Num 5, X], Mul [Num 3, X], X])  <-->  5*x + 3*x + x

-- combined (Mul [X, X, X, X, X, Mul [Num 5, X]])
