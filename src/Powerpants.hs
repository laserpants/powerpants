module Powerpants
  ( OGF(..)
  , ddx
  , expanded
  , expandedTo
  , lsh
  , rsh
  ) where

import Control.Monad
import Data.Ratio
import Foreign
import Math.Ginac.FFI
import Math.Ginac.FFI.ForeignPtr
import System.IO.Unsafe

data OGF = Gx (ForeignPtr GinacEx) | X

instance Num OGF where
    (+)          = Powerpants.add
    (*)          = Powerpants.mul
    negate       = Powerpants.neg
    abs          = Powerpants.abs
    signum       = Powerpants.sig
    fromInteger  = Powerpants.num

instance Fractional OGF where
    (/)          = Powerpants.div
    fromRational = Powerpants.rat

gx :: IO (Ptr GinacEx) -> OGF
gx ioptr = Gx (unsafePerformIO (ioptr >>= newForeignPtr ginac_ex_free_fun))

num :: Integral a => a -> OGF
num = gx . ginac_ex_new_from_int . fromIntegral

withOGF :: OGF -> (Ptr GinacEx -> IO b) -> IO b
withOGF (Gx ptr) fn = withForeignPtr ptr fn
withOGF        X fn = ginac_symbol_static >>= ginac_ex_new_from_basic >>= fn

binop :: (Ptr GinacEx -> Ptr GinacEx -> IO c) -> OGF -> OGF -> IO c
binop op f g = withOGF f (withOGF g . op)

add :: OGF -> OGF -> OGF
add ax bx = gx (binop ginac_add ax bx)

mul :: OGF -> OGF -> OGF
mul ax bx = gx (binop ginac_mul ax bx)

neg :: OGF -> OGF
neg ax = gx (withOGF ax ginac_ex_neg)

abs :: OGF -> OGF
abs ax = gx (withOGF ax ginac_ex_abs)

sig :: OGF -> OGF
sig ax = gx (withOGF ax ginac_ex_signum)

div :: OGF -> OGF -> OGF
div ax bx = gx (binop ginac_div ax bx)

pow :: OGF -> OGF -> OGF
pow ax bx = gx (binop ginac_pow ax bx)

rat :: Rational -> OGF
rat r = Powerpants.div (num n) (num d) where
    n = fromInteger (numerator r)
    d = fromInteger (denominator r)

ddx :: OGF -> OGF
ddx ax = gx (withOGF ax fn) where
    fn ptr = ginac_symbol_static
         >>= ginac_diff 1 ptr

factorial :: Int -> OGF
factorial n = gx (ginac_factorial n)

numcast :: Ptr GinacEx -> IO (Maybe Double)
numcast ptr = do
    isNumeric <- ginac_ex_is_numeric ptr
    if isNumeric
        then fmap Just (ginac_ex_to_double ptr)
        else pure Nothing

substitute :: OGF -> Int -> Maybe Double
substitute ax x = unsafePerformIO (withOGF ax fn) where
    fn ptr = ginac_symbol_static >>= ginac_ex_subs_int x ptr >>= numcast

expanded :: OGF -> [Integer]
expanded ax = maybe 0 round <$> series ax where
    series :: OGF -> [Maybe Double]
    series = rec 0 where
        rec n ax = x:xs where
            x  = substitute (ax / factorial n) 0
            xs = rec (succ n) (ddx ax)

expandedTo :: Int -> OGF -> [Integer]
expandedTo n = take n . expanded

lsh :: Int -> OGF -> OGF
lsh n ax = X^^negate n * ax

rsh :: Int -> OGF -> OGF
rsh n ax = X^^n * ax
