module Powerpants where

import Foreign
import Math.Ginac.FFI
import System.IO.Unsafe

data FunEx = FunEx
  !(ForeignPtr GinacEx)
  !(ForeignPtr GinacSymbol)

newFunEx :: Ptr GinacEx -> Ptr GinacSymbol -> IO FunEx
newFunEx expr symbol = do
    p <- newForeignPtr ginac_ex_free_fun expr
    q <- newForeignPtr ginac_basic_free_fun symbol
    pure (FunEx p q)

expanded :: FunEx -> [Integer]
expanded expr = [truncate (coefficient expr i) | i <- [0..]]

coefficient :: FunEx -> Int -> Double
coefficient (FunEx p q) n = 
    unsafePerformIO (withForeignPtr p (withForeignPtr q . impl)) where
    impl :: Ptr GinacEx 
         -> Ptr GinacSymbol 
         -> IO Double
    impl expr symbol = do
        varX <- ginac_ex_new_from_basic symbol
        zero <- ginac_ex_new_from_int 0
        rel  <- ginac_relation_eq_new varX zero -- x == 0
        series <- ginac_ex_series expr rel (succ n)
        c <- ginac_ex_coeff series varX n
        ginac_basic_free rel
        ginac_ex_free zero
        ginac_ex_free varX
        dbl <- ginac_ex_to_double c
        ginac_ex_free c
        ginac_ex_free series
        pure dbl
