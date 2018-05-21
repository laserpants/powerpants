{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Powerpants
  ( GF(..)
  , Powerpants.Ginac.diff
  , Powerpants.Ginac.eval
  , Powerpants.Ginac.factorial
  , Powerpants.Ginac.pow
  , Powerpants.Ginac.sqrt
  , x
  , printGF
  ) where

import Powerpants.Ginac

newtype GF = GF Expr deriving (Num, Fractional)

x :: GF
x = GF (Ex symbol)

printGF :: GF -> IO ()
printGF (GF ex) = printEx ex
