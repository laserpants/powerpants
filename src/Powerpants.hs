{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Powerpants where

import Math.Ginac
import Math.Ginac.Symbolic

newtype OGF = Gx Expr deriving (Num, Fractional)

