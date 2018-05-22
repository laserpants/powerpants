{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Powerpants
--  ( OGF(..)
  ( OGF(..)
  , Powerpants.diff
  , Powerpants.eval
  , Powerpants.toString
  , x
  ) where

import Powerpants.Ginac ( Expr(..) )

import qualified Powerpants.Ginac as Ex

newtype OGF = Gx Expr deriving (Num, Fractional)

x :: OGF
x = Gx (Ex Ex.symbol)

eval :: OGF -> Int -> Maybe Double
eval (Gx ex) = Ex.eval ex

diff :: OGF -> OGF
diff (Gx ex) = Gx (Ex.diff ex)

toString :: OGF -> String
toString (Gx ex) = Ex.toString ex
