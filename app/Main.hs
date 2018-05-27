module Main where

--import Foreign
--import Foreign.C.String
--import Math.Ginac.FFI
--import Powerpants

import Data.Ratio
import Powerpants
import Powerpants.Expr

instance Num Expr where
    a + b        = Add a b
    a - b        = Sub a b
    a * b        = Mul a b
    abs          = Abs
    negate       = Neg
    signum       = Sign
    fromInteger  = Num . fromIntegral

instance Fractional Expr where
    a / b        = Div a b
    fromRational = ratio

ratio :: Rational -> Expr
ratio r = Div (Num n) (Num d) where
    n = fromInteger (numerator r)
    d = fromInteger (denominator r)

main :: IO ()
main = do

    --symbol_x     <- withCString "x" ginac_symbol_new
    --ex_x         <- ginac_ex_new_from_basic symbol_x     -- x
    --ex_1         <- ginac_ex_new_from_int 1              -- 1
    --ex_2         <- ginac_ex_new_from_int 2              -- 2
    --ex_neg_x     <- ginac_ex_neg ex_x                    -- (-x)
    --ex_1_sub_x   <- ginac_ex_add ex_1 ex_neg_x           -- 1-x
    --ex_x_sqr     <- ginac_ex_pow ex_x ex_2               -- x^2
    --ex_neg_x_sqr <- ginac_ex_neg ex_x_sqr                -- (-x^2)
    --ex_denom     <- ginac_ex_add ex_1_sub_x ex_neg_x_sqr -- (1-x-x^2)
    --ex_frac      <- ginac_ex_div ex_x ex_denom           -- x/(1-x-x^2)

    -- let gx = Div X (Sub (Sub (Num 1) X) (Pow X (Num 2)))
    let gx = X/(1 - X - X^2)
    expr <- newEx gx
    mapM_ print (take 80 (expanded expr))

    --expr <- newFunEx ex_frac symbol_x
    --mapM_ print (take 800 (expanded expr))

    --ginac_ex_free ex_denom
    --ginac_ex_free ex_neg_x_sqr
    --ginac_ex_free ex_x_sqr
    --ginac_ex_free ex_1_sub_x
    --ginac_ex_free ex_neg_x
    --ginac_ex_free ex_2
    --ginac_ex_free ex_1
    --ginac_ex_free ex_x
