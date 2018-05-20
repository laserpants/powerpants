{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Powerpants
  ( GF(..)
  , Powerpants.Ginac.diff
  , Powerpants.Ginac.eval
  , x
  , printGF
  ) where

import Powerpants.Ginac

newtype GF = GF Expr deriving (Num)

x :: Expr
x = Ex symbol

printGF :: GF -> IO ()
printGF (GF ex) = printEx ex
