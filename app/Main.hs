module Main where

import Foreign
import Foreign.C.String
import Math.Ginac.FFI
import Powerpants

main :: IO ()
main = do

    symbol_x     <- withCString "x" ginac_symbol_new
    ex_x         <- ginac_ex_new_from_basic symbol_x     -- x
    ex_1         <- ginac_ex_new_from_int 1              -- 1
    ex_2         <- ginac_ex_new_from_int 2              -- 2
    ex_neg_x     <- ginac_ex_neg ex_x                    -- (-x)
    ex_1_sub_x   <- ginac_ex_add ex_1 ex_neg_x           -- 1-x
    ex_x_sqr     <- ginac_ex_pow ex_x ex_2               -- x^2
    ex_neg_x_sqr <- ginac_ex_neg ex_x_sqr                -- (-x^2)
    ex_denom     <- ginac_ex_add ex_1_sub_x ex_neg_x_sqr -- (1-x-x^2)
    ex_frac      <- ginac_ex_div ex_x ex_denom           -- x/(1-x-x^2)

    e <- newFunEx ex_frac symbol_x
    mapM_ print (take 800 (expanded e))

    ginac_ex_free ex_denom
    ginac_ex_free ex_neg_x_sqr
    ginac_ex_free ex_x_sqr
    ginac_ex_free ex_1_sub_x
    ginac_ex_free ex_neg_x
    ginac_ex_free ex_2
    ginac_ex_free ex_1
    ginac_ex_free ex_x

    pure ()
