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

import qualified Powerpants.Ginac as G

newtype OGF = Gx Expr deriving (Num, Fractional)

x :: OGF
x = Gx (Ex G.symbol)

eval :: OGF -> Int -> Maybe Double
eval (Gx ex) = G.eval ex

diff :: OGF -> OGF
diff (Gx ex) = Gx (G.diff ex)

toString :: OGF -> String
toString (Gx ex) = G.toString ex
