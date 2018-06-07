{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Powerpants.Expr.Symbolic where

import Algebra.Additive
import Algebra.Field
import Algebra.Ring
import NumericPrelude
import Powerpants.Expr

instance Algebra.Ring.C a => Algebra.Additive.C (Expr a) where
    a + b       = Add [a, b]
    zero        = Num 0
    negate      = neg

instance Algebra.Ring.C a => Algebra.Ring.C (Expr a) where
    a * b       = Mul [a, b]
    fromInteger = Num . fromInteger

instance Algebra.Ring.C a => Algebra.Field.C (Expr a) where
    recip exp   = Pow exp (-1)
