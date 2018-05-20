{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Powerpants
  ( GF(..)
  , x
  , printGF
  ) where

import Powerpants.Ginac

newtype GF = GF Expr deriving (Num)

x :: Expr
x = Ex symbol

printGF :: GF -> IO ()
printGF (GF ex) = printEx ex
