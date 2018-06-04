{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Main where

import Algebra.Ring
import Data.List
import Debug.Trace
import NumericPrelude
import NumericPrelude.Base
import Powerpants.Expr

--partitionMuls :: [Expr a] -> ([Expr a], [Expr a])
--partitionMuls = rec ([], []) where
--    rec p [] = p
--    rec (exprs, xs') (x:xs) = rec p' xs where
--        p' = case x of
--               Mul xs -> (xs ++ exprs, xs')
--               _      -> (exprs, x:xs')

main :: IO ()
main = return ()
