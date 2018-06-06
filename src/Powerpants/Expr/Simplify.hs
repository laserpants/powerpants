{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Powerpants.Expr.Simplify where

import Algebra.Ring
import Data.List           ( sort )
import NumericPrelude
import Powerpants.Expr

-- | Recursively flatten (or /level/) nested addition and multiplication nodes.
flattened :: Expr a -> Expr a
flattened expr =
    case expr of
      Mul xs -> Mul (level unwrapMul xs)
      Add xs -> Add (level unwrapAdd xs)
      _      -> expr
  where
    level :: (Expr a -> Maybe [Expr a]) -> [Expr a] -> [Expr a]
    level fn = rec [] where
        rec exs' [] = exs'
        rec exs' (ex:exs) = rec res exs where
            res = case fn (flattened ex) of
              Just xs -> xs ++ exs'
              Nothing -> ex:exs'

-- | Partition a list of 'Expr's into two lists; one with all constants (i.e.,
--   numeric values wrapped in 'Num' constructors), and another with the
--   remaining expressions.
collectConsts :: [Expr a] -> ([a], [Expr a])
collectConsts = rec ([], []) where
    rec p [] = p
    rec (nums, xs') (x:xs) = rec p' xs where
        p' = case x of
               Num n -> (n:nums, xs')
               _     -> (nums, x:xs')

-- | Replace empty addition and multiplication nodes with zero or one (the
--   operation's identity), and extract the expression from lists with only one
--   element.
applyId :: (Algebra.Ring.C a) => Expr a -> Expr a
applyId (Add [ ]) = Num 0
applyId (Add [x]) = x
applyId (Mul [ ]) = Num 1
applyId (Mul [x]) = x
applyId expr      = expr

normalOrder :: (Algebra.Ring.C a, Ord a) => [Expr a] -> [Expr a]
normalOrder = sort . fmap combined

-- | Combine constants under an addition or multiplication node by adding or
--   multiplying them into a single 'Num' value.
combined :: (Algebra.Ring.C a, Ord a) => Expr a -> Expr a
combined (Add xs) = applyId (Add (normalOrder exprs)) where
    (nums, rest) = collectConsts xs
    exprs = case sum nums of
              0 -> rest
              n -> Num n:rest
combined (Mul xs) = applyId (Mul (normalOrder exprs)) where
    (nums, rest) = collectConsts xs
    exprs = case product nums of
              0 -> []
              1 -> rest
              n -> Num n:rest
combined (Pow a n) = Pow (combined a) n
combined expr = expr

-- | Eliminate repeated powers and evaluate powers of constants if the result
--   is less than 5000.
levelled :: (Algebra.Ring.C a, Ord a) => Expr a -> Expr a
levelled pow@(Pow (Num 0) 0) = pow -- To avoid having to think about 0^0
levelled (Pow (Num 0) n)     = Num 0
levelled (Pow a 0)           = Num 1
levelled (Pow a 1)           = levelled a
levelled pow@(Pow (Num a) n) = let m = a^n in if m < 5000 then Num m else pow
levelled (Pow (Pow a m) n)   = Pow (levelled a) (m*n)
levelled expr                = expr

-- collectMatching = undefined

simplified :: (Algebra.Ring.C a, Ord a) => Expr a -> Expr a
simplified = combined . levelled . flattened

expr1 :: Algebra.Ring.C a => Expr a
expr1 = Add
  [ Mul [Num 5, X]
  , Pow X (-10)
  , Pow X 3
  --, Div (Mul [Num 2, Pow X 2]) (Num 3)
  , Num 7 ]

-- groupSimilar :: (Ord a, Eq a) => [Expr a] -> [(Expr a, Int)]
-- groupSimilar = foldr fn [] where
--     fn (Num n) al = (Num n, 1):al  -- Leave constants to be folded later
--     fn expr al =
--         case lookup expr al of
--           Just n  -> insert (expr, succ n) (filter ((/= expr) . fst) al)
--           Nothing -> insert (expr, 1) al
--
-- combineUsing :: (Algebra.Ring.C a, Ord a) => ((Expr a, Int) -> Expr a) -> [Expr a] -> [Expr a]
-- combineUsing fn xs = fmap (combined . fn) (groupSimilar xs)
--
-- -- | Collect and combine like expressions.
-- combined :: (Algebra.Ring.C a, Ord a, Eq a) => Expr a -> Expr a
-- combined (Mul xs) = Mul (combineUsing fn xs) where
--     fn (expr, 0)        = Num 1
--     fn (expr, 1)        = expr
--     fn (Pow a n, count) = Pow a (n * fromIntegral count)
--     fn (expr, count)    = Pow expr (fromIntegral count)
-- combined (Add xs) = Add (combineUsing fn xs) where
--     fn (expr, 0)        = Num 0
--     fn (expr, 1)        = expr
--     fn (expr, count)    = Mul [Num (fromIntegral count), expr]
-- combined expr = expr
--
-- -- combined (Mul [Pow X 3, Pow X 5, X, X])  <-->  x^3 * x^5 * x * x
--
-- -- combined (Add [Pow X 3, Pow X 5, X, X])  <-->  x^3 + x^5 + x + x
--
-- -- combined (Add [Mul [Num 5, X], Mul [Num 3, X], X])  <-->  5*x + 3*x + x
--
-- -- combined (Mul [X, X, X, X, X, Mul [Num 5, X]])
