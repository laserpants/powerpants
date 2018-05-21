{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Powerpants
  ( OGF(..)
  , Powerpants.Ginac.diff
  , Powerpants.Ginac.eval
  , Powerpants.Ginac.factorial
  , Powerpants.Ginac.pow
  , Powerpants.Ginac.sqrt
  , x
  , printGF
  ) where

import Powerpants.Ginac

newtype OGF = OGF Expr deriving (Num, Fractional)

x :: OGF
x = OGF (Ex symbol)

printGF :: OGF -> IO ()
printGF (OGF ex) = printEx ex
