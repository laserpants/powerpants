{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Main where

import Algebra.Ring
import Data.List
import Debug.Trace
import NumericPrelude
import NumericPrelude.Base
import Powerpants.Expr

partitionMuls :: [Expr a] -> ([Expr a], [Expr a])
partitionMuls = rec ([], []) where
    rec p [] = p
    rec (exprs, xs') (x:xs) = rec p' xs where
        p' = case x of
               Mul xs -> (xs ++ exprs, xs')
               _      -> (exprs, x:xs')

-- | Transform the syntax tree so that a division node cannot be the immediate
--   child of either a division node or a multiplication node.
divnode :: Expr a -> Expr a
divnode (Div (Div a b) c) = Div a (Mul [b, c])
divnode (Div a (Div b c)) = Div (Mul [a, c]) b
-- todo
divnode expr = expr

main :: IO ()
main = return ()
