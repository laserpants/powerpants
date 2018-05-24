{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Powerpants where

import Math.Ginac
import Math.Ginac.FFI
import Math.Ginac.Symbolic
import System.IO.Unsafe

newtype OGF = Gx Expr deriving (Num, Fractional)

factorial :: Int -> OGF
factorial n = undefined

derivative :: OGF -> OGF
derivative (Gx ex) = undefined

substitute :: OGF -> Int -> Maybe Double
substitute = undefined

coefficient :: OGF -> Int -> Integer
coefficient gf nth = expand gf !! nth

expand :: OGF -> [Integer]
expand = fmap cast . rec 0 where
    cast :: Maybe Double -> Integer
    cast = maybe 0 round
    rec :: Int -> OGF -> [Maybe Double]
    rec n gf = x:xs where
        x    = substitute (gf / Powerpants.factorial n) 0
        xs   = rec (succ n) (derivative gf)

xsym :: Symbol
xsym = undefined

x :: OGF
x = undefined
