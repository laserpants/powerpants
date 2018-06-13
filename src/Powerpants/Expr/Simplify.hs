{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Powerpants.Expr.Simplify where

import Algebra.Ring
import Algebra.ToInteger
import Data.Functor        ( (<$>) )
import Data.List           ( sort, insert )
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
--   numeric values wrapped in 'Num' constructors), and another with remaining
--   expressions.
collectConsts :: [Expr a] -> ([a], [Expr a])
collectConsts = rec ([], []) where
    rec p [] = p
    rec (nums, xs') (x:xs) = rec p' xs where
        p' = case x of
               Num n -> (n:nums, xs')
               _     -> (nums, x:xs')

-- | Replace empty addition and multiplication nodes with zero or one (the
--   operation's identity). Extract the expression from lists with only one
--   element.
applyId :: (Algebra.Ring.C a) => Expr a -> Expr a
applyId (Add [ ]) = Num 0
applyId (Add [x]) = x
applyId (Mul [ ]) = Num 1
applyId (Mul [x]) = x
applyId expr      = expr

-- | Combine constants under addition and multiplication nodes by adding or
--   multiplying them into a single 'Num' value.
combined :: (Algebra.Ring.C a, Ord a) => Expr a -> Expr a
combined (Add xs) = applyId (Add (fmap combined exprs)) where
    (nums, rest) = collectConsts xs
    exprs = case sum nums of
              0 -> rest
              n -> Num n:rest
combined (Mul xs) = applyId (Mul (fmap combined exprs)) where
    (nums, rest) = collectConsts xs
    exprs = case product nums of
              0 -> [Num 0]
              1 -> rest
              n -> Num n:rest
combined (Pow a n) = Pow (combined a) n
combined expr = expr

-- | Eliminate repeated powers and evaluate powers of constants if the result
--   is less than 5000.
compressed :: (Algebra.Ring.C a, Ord a) => Expr a -> Expr a
compressed pow@(Pow (Num 0) 0) = pow -- To avoid having to think about 0^0
compressed (Pow (Num 0) n)     = Num 0
compressed (Pow a 0)           = Num 1
compressed (Pow a 1)           = compressed a
compressed pow@(Pow (Num a) n) = let m = a^n in if m < 5000 then Num m else pow
compressed (Pow (Pow a m) n)   = Pow (compressed a) (m*n)
compressed expr                = expr

type AssocList k v = [(k, v)]

increment :: (Ord a)
          => Expr a
          -> Int
          -> AssocList (Expr a) Int
          -> AssocList (Expr a) Int
increment expr m al =
    case lookup expr al of
      Just n  -> insert (expr, n + m) (filter ((/= expr) . fst) al)
      Nothing -> insert (expr, m) al

collectTerms :: (Algebra.ToInteger.C a, Ord a)
             => [Expr a]
             -> AssocList (Expr a) Int
collectTerms = foldr fn [] where
    fn (Num n)  al = (Num n, 1) : al  -- Leave constants for now
    fn (Mul xs) al =
        case sort xs of
          (Num n:xs) -> increment (Mul xs) (fromIntegral n) al
          xs'        -> increment (Mul xs') 1 al
    fn expr al = increment expr 1 al

collectAlike :: (Algebra.ToInteger.C a, Ord a)
             => [Expr a]
             -> AssocList (Expr a) Int
collectAlike = foldr fn [] where
    fn (Num n)   al = (Num n, 1) : al  -- Leave constants for now
    fn (Pow a n) al = increment a (fromIntegral n) al
    fn expr      al = increment expr 1 al

collected :: (Algebra.ToInteger.C a, Ord a) => Expr a -> Expr a
collected (Add xs) = Add (collected . expand <$> collectTerms xs) where
    expand (x, 1) = x
    expand (x, c) = Mul [Num (fromIntegral c), x]
collected (Mul xs) = Mul (collected . expand <$> collectAlike xs) where
    expand (x, 1) = x
    expand (x, n) = Pow (collected x) (fromIntegral n)
collected (Pow a n) = Pow (collected a) n
collected expr = expr

simplified :: (Algebra.ToInteger.C a, Ord a) => Expr a -> Expr a
simplified = flattened . combined . compressed . collected

simplified' :: (Algebra.ToInteger.C a, Ord a) => Expr a -> Expr a
simplified' expr =
    let expr' = simplified expr
     in if expr' == expr
            then expr'
            else simplified' expr'
