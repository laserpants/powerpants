{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Powerpants
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
{-# INLINE x #-}
x = Gx (Ex Ex.symbol)

eval :: OGF -> Int -> Maybe Double
{-# INLINE eval #-}
eval (Gx ex) = Ex.eval ex

diff :: OGF -> OGF
{-# INLINE diff #-}
diff (Gx ex) = Gx (Ex.diff ex)

toString :: OGF -> String
{-# INLINE toString #-}
toString (Gx ex) = Ex.toString ex
