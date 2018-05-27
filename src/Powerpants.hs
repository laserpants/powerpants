module Powerpants where

import Control.Monad ( (>=>) )
import Foreign
import Math.Ginac.FFI
import System.IO.Unsafe

f123 :: FunEx -> [Double]
f123 (FunEx p q) = rec 0 where
    rec n = unsafePerformIO (withForeignPtr p (withForeignPtr q . baaz)) : rec (succ n) where
      --baaz :: IO Double
      baaz ex s = do
        x   <- ginac_ex_new_from_basic s
        nil <- ginac_ex_new_from_int 0
        rel <- ginac_relation_eq_new x nil
        ginac_ex_free nil
        ginac_ex_free x
        sxp <- ginac_ex_series ex rel (succ n)
        ginac_basic_free rel
        ginac_ex_coeff_symbol n sxp s >>= ginac_ex_to_double

        --pure (Series sxp q upto)

        --(Series p q _) <- yyyy funex (n + 1)
        --withForeignPtr p (withForeignPtr q . (ginac_ex_coeff_symbol n p q >>= ginac_ex_to_double))

--        rec n = unsafePerformIO (withForeignPtr p (\p' -> withForeignPtr q (\q' -> baz p' q'))) : rec (succ n) where
--          baz :: Ptr GinacEx -> Ptr GinacSymbol -> IO Double
--          baz p q = ginac_ex_coeff_symbol n p q >>= ginac_ex_to_double

data FunEx = FunEx
  !(ForeignPtr GinacEx)
  !(ForeignPtr GinacSymbol)

data Series = Series 
  !(ForeignPtr GinacEx) 
  !(ForeignPtr GinacSymbol)
  !Int

baaaz (FunEx p q) = [truncate $ getCoeff i p q | i <- [0..]]

getCoeff :: Int -> ForeignPtr GinacEx -> ForeignPtr GinacSymbol -> Double
getCoeff n p q = do
    unsafePerformIO (withForeignPtr p (withForeignPtr q . fun))
  where
    fun ex s = do
      x   <- ginac_ex_new_from_basic s
      nil <- ginac_ex_new_from_int 0
      rel <- ginac_relation_eq_new x nil
      sxp <- ginac_ex_series ex rel (n + 1)
      ginac_basic_free rel
      ginac_ex_free nil
      ginac_ex_free x
      c <- ginac_ex_coeff_symbol n sxp s 
      dbl <- ginac_ex_to_double c
      ginac_ex_free c
      ginac_ex_free sxp
      pure dbl

xxxx :: Ptr GinacEx -> Ptr GinacSymbol -> IO FunEx
xxxx ex s = do
    p <- newForeignPtr ginac_ex_free_fun ex
    q <- newForeignPtr ginac_basic_free_fun s
    pure (FunEx p q)

yyyy :: FunEx -> Int -> IO Series
yyyy (FunEx p q) upto = withForeignPtr p (withForeignPtr q . series) 
  where
    series ex s = do
      x   <- ginac_ex_new_from_basic s
      nil <- ginac_ex_new_from_int 0
      rel <- ginac_relation_eq_new x nil
      ginac_ex_free nil
      ginac_ex_free x
      sxp <- ginac_ex_series ex rel upto >>= newForeignPtr ginac_ex_free_fun
      ginac_basic_free rel
      pure (Series sxp q upto)

bazz2 :: FunEx -> [Double]
bazz2 ax = fez 0 where
    fez n = unsafePerformIO (abab n) ++ fez (n + 1)
    abab :: Int -> IO [Double]
    abab to = do
        serex <- yyyy ax to
        pure (zzzz serex)
      where
        zzzz :: Series -> [Double]
        zzzz (Series p q _) = rec 0 where
            rec :: Int -> [Double]
            rec n | n >= to = []
            rec n = unsafePerformIO (withForeignPtr p (\p' -> withForeignPtr q (\q' -> baz p' q'))) : rec (succ n) where
              baz :: Ptr GinacEx -> Ptr GinacSymbol -> IO Double
              baz p q = ginac_ex_coeff_symbol n p q >>= ginac_ex_to_double



--    rec n = unsafePerformIO (withForeignPtr p (withForeignPtr q . coeff n)) : rec (succ n)
--    coeff n ex s = do
--        x   <- ginac_ex_new_from_basic s
--        nil <- ginac_ex_new_from_int 0
--        rel <- ginac_relation_eq_new x nil
--        sxp <- ginac_ex_series ex rel (succ n)
--        ginac_basic_free rel
--        ginac_ex_free nil
--        ginac_ex_free x
--        ginac_ex_coeff_symbol n sxp s >>= ginac_ex_to_double

series :: FunEx -> IO (Ptr GinacEx)
series = undefined

coeff :: Int -> Ptr GinacEx -> IO Double
coeff = undefined

bazz :: FunEx -> [Double]
bazz ax = xxx 0 1 where
    xxx from to = unsafePerformIO (abab ax from to) ++ xxx to (to + 1)

abab :: FunEx -> Int -> Int -> IO [Double]
abab funex from to = do
    serex <- yyyy funex to
    pure (zzzz serex)
  where
    zzzz :: Series -> [Double]
    zzzz (Series p q m) = rec 0 where
        rec :: Int -> [Double]
        rec n | n >= to = []
        rec n | otherwise = unsafePerformIO (withForeignPtr p (\p' -> withForeignPtr q (\q' -> baz p' q'))) : rec (succ n) where
          baz :: Ptr GinacEx -> Ptr GinacSymbol -> IO Double
          baz p q = ginac_ex_coeff_symbol n p q >>= ginac_ex_to_double


--yyy p q = rec 0 where
--    rec n = (ginac_ex_coeff_symbol n p q >== ginac_ex_to_double) : rec (succ n)



--data SeriesExpansion = Expansion
--  !(ForeignPtr GinacEx)
--  !(ForeignPtr GinacSymbol)
--  !Int

--yyy :: Ptr GinacEx -> IO SeriesExpansion
--yyy ex = do
--    ex0 <- ginac_ex_new_from_int 0
--    rel <- ginac_relation_eq_new ex_x ex0

--yyy :: Ptr GinacEx -> Ptr GinacSymbol -> IO [Double]
--yyy p q = rec 0 where
--    rec n = (ginac_ex_coeff_symbol n p q >== ginac_ex_to_double) : rec (succ n)

--zzz :: SeriesExpansion -> [Double]
--zzz = undefined

xxx :: ForeignPtr GinacEx -> ForeignPtr GinacSymbol -> [Double]
xxx p q = rec 0 where
    rec n = unsafePerformIO (withForeignPtr p (\p' -> withForeignPtr q (\q' -> baz p' q'))) : rec (succ n) where
      baz p q = ginac_ex_coeff_symbol n p q >>= ginac_ex_to_double

--expand :: ForeignPtr GinacEx -> ForeignPtr GinacSymbol -> [Double]
--expand = undefined
--
-- module Powerpants where
-- --  ( OGF(..)
-- --  , coefficient
-- --  , ddx
-- --  , expand
-- --  , nth
-- --  ) where
-- 
-- import Control.Monad
-- import Data.Ratio
-- import Foreign
-- import Math.Ginac.FFI
-- --import Math.Ginac.FFI.ForeignPtr
-- import System.IO.Unsafe
-- 
-- data OGF = Gx (ForeignPtr GinacEx) | X
-- 
-- instance Num OGF where
--     (+)          = Powerpants.add
--     (*)          = Powerpants.mul
--     negate       = Powerpants.neg
--     abs          = Powerpants.abs
--     signum       = Powerpants.sig
--     fromInteger  = Powerpants.num
-- 
-- instance Fractional OGF where
--     (/)          = Powerpants.div
--     fromRational = Powerpants.rat
-- 
-- instance Eq OGF where
--     (==)         = Powerpants.eql
-- 
-- gx :: IO (Ptr GinacEx) -> OGF
-- gx ioptr = Gx (unsafePerformIO (ioptr >>= newForeignPtr ginac_ex_free_fun))
-- 
-- num :: Integral a => a -> OGF
-- num = gx . ginac_ex_new_from_int . fromIntegral
-- 
-- withExPtr :: OGF -> (Ptr GinacEx -> IO b) -> IO b
-- withExPtr (Gx ptr) fn = withForeignPtr ptr fn
-- withExPtr X fn = ginac_symbol_static >>= ginac_ex_new_from_basic >>= fn
-- 
-- binop :: (Ptr GinacEx -> Ptr GinacEx -> IO c) -> OGF -> OGF -> IO c
-- binop op f g = withExPtr f (withExPtr g . op)
-- 
-- add :: OGF -> OGF -> OGF
-- add ax bx = gx (binop ginac_ex_add ax bx)
-- 
-- mul :: OGF -> OGF -> OGF
-- mul ax bx = gx (binop ginac_ex_mul ax bx)
-- 
-- neg :: OGF -> OGF
-- neg ax = gx (withExPtr ax ginac_ex_neg)
-- 
-- abs :: OGF -> OGF
-- abs ax = gx (withExPtr ax ginac_ex_abs)
-- 
-- sig :: OGF -> OGF
-- sig ax = gx (withExPtr ax ginac_ex_signum)
-- 
-- div :: OGF -> OGF -> OGF
-- div ax bx = gx (binop ginac_ex_div ax bx)
-- 
-- pow :: OGF -> OGF -> OGF
-- pow ax bx = gx (binop ginac_ex_pow ax bx)
-- 
-- rat :: Rational -> OGF
-- rat r = Powerpants.div (num n) (num d) where
--     n = fromInteger (numerator r)
--     d = fromInteger (denominator r)
-- 
-- eql :: OGF -> OGF -> Bool
-- eql ax bx = unsafePerformIO (withExPtr ax (withExPtr bx . ginac_ex_equal))
-- 
-- ddx :: OGF -> OGF
-- ddx ax = gx (withExPtr ax fn) where
--     fn ptr = ginac_symbol_static
--          >>= ginac_ex_diff 1 ptr
-- 
-- factorial :: Int -> OGF
-- factorial n = gx (ginac_ex_factorial n)
-- 
-- 
-- 
-- yyy :: OGF -> [IO Double]
-- yyy ax = unsafePerformIO (withExPtr ax f) where
--   f ptr = do
--     x <- ginac_symbol_static >>= ginac_ex_new_from_basic
--     nil <- ginac_ex_new_from_int 0
--     rel <- ginac_relation_eq_new x nil
--     series <- ginac_ex_series ptr rel 4000
--     pure [ginac_ex_coeff series x i >>= ginac_ex_to_double | i <- [0..]]
-- 
-- --unsafePerformIO (withExPtr ax fun)
-- --  where
-- --    fun :: Ptr GinacEx -> IO [IO Int]
-- --    fun ptr = let prod = g ptr in pure [prod i | i <- [0..100]]
-- --    g :: Ptr GinacEx -> Int -> IO Int
-- --    g ptr n = do
-- --        x <- ginac_symbol_static >>= ginac_ex_new_from_basic
-- --        nil <- ginac_ex_new_from_int 0
-- --        rel <- ginac_relation_eq_new x nil
-- --        series <- ginac_ex_series ptr rel 400
-- --        c <- ginac_ex_coeff series x n
-- --        ginac_ex_to_int c
-- 
-- 
-- 
-- 
-- --------------------------------------------
-- --------------------------------------------
-- --------------------------------------------
-- --------------------------------------------
-- --------------------------------------------
-- --------------------------------------------
-- --------------------------------------------
-- 
-- 
-- --zzzzz :: OGF -> [Maybe Double]
-- --zzzzz ax = undefined where -- unsafePerformIO (withExPtr ax baz) where
-- --    baz :: Ptr GinacEx -> IO [IO (Maybe Double)]
-- --    baz ptr = do
-- --        x <- ginac_symbol_static
-- --        p <- ginac_ex_new_from_basic x
-- --        z <- ginac_ex_new_from_int 0
-- --        r <- ginac_relation_eq_new p z
-- --        s <- ginac_ex_series ptr r 400
-- --        ginac_basic_free r
-- --        --pure [ginac_ex_coeff_symbol j s x >>= numcast | j <- [0 .. n]]
-- 
-- 
-- baz :: Int -> Ptr GinacEx -> Ptr GinacSymbol -> [IO (Ptr GinacEx)]
-- baz n s x = ginac_ex_coeff_symbol n s x : baz (n + 1) s x
-- 
-- fez :: OGF -> [IO (Ptr GinacEx)]
-- fez ax = unsafePerformIO (withExPtr ax fn) where
--     fn ptr = do 
--         x <- ginac_symbol_static
--         p <- ginac_ex_new_from_basic x
--         z <- ginac_ex_new_from_int 0
--         r <- ginac_relation_eq_new p z
--         s <- ginac_ex_series ptr r 400
--         pure (baz 0 s x)
-- 
-- --asdfasfd :: OGF -> [Ptr GinacEx]
-- asdfasfd ax = fmap (unsafePerformIO . numcast . unsafePerformIO) (fez ax)
-- 
-- slafs ax = sequence (fez ax)
-- 
-- coefficient :: OGF -> Int -> Maybe Double
-- coefficient ax n = unsafePerformIO (withExPtr ax fn) where
--     fn :: Ptr GinacEx -> IO (Maybe Double)
--     fn ptr = do 
--         x <- ginac_symbol_static
--         p <- ginac_ex_new_from_basic x
--         z <- ginac_ex_new_from_int 0
--         r <- ginac_relation_eq_new p z
--         s <- ginac_ex_series ptr r 600
--         ginac_basic_free r
--         a <- ginac_ex_coeff_symbol n s x
--         ginac_ex_free s
--         ginac_ex_free z
--         ginac_ex_free p
--         numcast a
-- 
-- expnd4 :: OGF -> [Maybe Double]
-- expnd4 ax = unsafePerformIO (withExPtr ax f)
--   where
--     f ptr = do
--         x <- ginac_symbol_static
--         p <- ginac_ex_new_from_basic x
--         z <- ginac_ex_new_from_int 0
--         r <- ginac_relation_eq_new p z
--         s <- ginac_ex_series ptr r 900
-- --        ss <- newForeignPtr ginac_ex_free_fun s
-- 
-- --        ginac_basic_free r
-- --        ginac_ex_free z
-- --        ginac_ex_free p
-- 
--         pure (expnd3 ptr x)
-- 
-- expnd3' :: ForeignPtr GinacEx -> Ptr GinacSymbol -> [Double]
-- expnd3' ptr x = rec 0 where
--     rec n = unsafePerformIO (withForeignPtr ptr 
--       (\expr -> ginac_ex_coeff_symbol n expr x >>= ginac_ex_to_double)) : rec (n + 1) 
-- 
-- expnd3 :: Ptr GinacEx -> Ptr GinacSymbol -> [Maybe Double]
-- expnd3 expr x = rec 0 where
--     rec n = unsafePerformIO (ginac_ex_coeff_symbol n expr x >>= numcast) : rec (n + 1) 
-- 
-- expnd2 :: OGF -> [Double]
-- expnd2 ax = expnd3' s x where
--     (s, x) = unsafePerformIO (withExPtr ax baz)
--     baz ptr = do
--         x <- ginac_symbol_static
-- 
--         p <- ginac_ex_new_from_basic x
--         --pp <- newForeignPtr ginac_ex_free_fun p
-- 
--         z <- ginac_ex_new_from_int 0
--         --zz <- newForeignPtr ginac_ex_free_fun z
-- 
--         r <- ginac_relation_eq_new p z
--         --rr <- newForeignPtr ginac_basic_free_fun r
-- 
--         --s <- ginac_ex_series ptr r 900 >>= newForeignPtr ginac_ex_free_fun 
--         s <- ginac_ex_series ptr r 900 >>= newForeignPtr ginac_ex_free_fun
--         --ss <- newForeignPtr ginac_ex_free_fun s
-- 
--         ginac_basic_free r
--         ginac_ex_free z
--         ginac_ex_free p
-- 
--         --withForeignPtr s $ \ptr -> pure (f ptr x 0)
-- 
--         pure (s, x) -- (f s x 0)
--         -- How to free these pointers?
--         --ginac_ex_free s
--         -- ginac_basic_free r
--         -- ginac_ex_free z
--         -- ginac_ex_free p
-- 
--     --ff :: Ptr GinacEx -> Ptr GinacSymbol -> Int -> [IO (Maybe Double)]
--     --ff s x n = (ginac_ex_coeff_symbol n s x >>= numcast) : ff s x (n + 1) 
-- 
-- fun :: Ptr GinacEx -> Ptr GinacSymbol -> Int -> [Maybe Double]
-- fun s x n = unsafePerformIO (ginac_ex_coeff_symbol n s x >>= numcast) : fun s x (n + 1) 
-- 
-- expnd :: OGF -> Int -> [Maybe Double]
-- expnd ax n = unsafePerformIO (withExPtr ax baz) where
--     baz :: Ptr GinacEx -> IO [Maybe Double]
--     baz ptr = do
--         x <- ginac_symbol_static
--         p <- ginac_ex_new_from_basic x
--         z <- ginac_ex_new_from_int 0
--         r <- ginac_relation_eq_new p z
--         s <- ginac_ex_series ptr r 500
--         -- ginac_basic_free r
--         -- a <- ginac_ex_coeff_symbol n s x
--         --ginac_ex_free s
--         --ginac_ex_free z
--         -- ginac_ex_free p
--         sequence [ginac_ex_coeff_symbol j s x >>= numcast | j <- [0 .. n]]
--     --baz :: Ptr GinacEx -> IO [Maybe Double]
--     --baz ptr = sequence [fn ptr i | i <- [0 .. n]]
--     --fn :: Ptr GinacEx -> Int -> IO (Maybe Double)
--     --fn ptr i = do
--     --    sy <- ginac_symbol_static 
--     --    y <- ginac_ex_coeff_symbol i ptr sy
--     --    numcast y
-- 
-- showGF :: OGF -> IO ()
-- showGF ax = withExPtr ax ginac_ex_print
-- 
numcast :: Ptr GinacEx -> IO (Maybe Double)
numcast ptr = do
    isNumeric <- ginac_ex_is_numeric ptr
    if isNumeric
        then fmap Just (ginac_ex_to_double ptr)
        else pure Nothing

--
-- --
-- --
-- 
-- substitute :: OGF -> Int -> Maybe Double
-- substitute ax x = unsafePerformIO (withExPtr ax fn) where
--     fn ptr = ginac_symbol_static >>= ginac_ex_subs_int x ptr >>= numcast
-- 
-- series :: OGF -> [Maybe Double]
-- series = rec 0 where
--     rec n ax = x:xs where
--         x  = substitute (ax / factorial n) 0
--         xs = rec (succ n) (ddx ax)
-- 
-- --expand :: Int -> OGF -> Maybe [Integer]
-- --expand n = (fmap . fmap) truncate . sequence . take n . series
-- 
-- expand n = take n . series
-- 
-- --nth :: Int -> OGF -> Maybe Integer
-- --nth n = fmap last . expand (succ n)
-- nth n ax = Just 0
