{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Powerpants
  ( OGF(..)
  , Powerpants.Ginac.diff
  , Powerpants.Ginac.subs
  , Powerpants.Ginac.factorial
  , Powerpants.Ginac.pow
  , Powerpants.Ginac.sqrt
  , Powerpants.eval
  , x
  , printGF
  ) where

import Powerpants.Ginac

newtype OGF = OGF Expr deriving (Num, Fractional)

x :: OGF
x = OGF (Ex symbol)

eval :: OGF -> Int -> Maybe Double
eval (OGF ex) = Powerpants.Ginac.eval ex

printGF :: OGF -> IO ()
printGF (OGF ex) = printEx ex
