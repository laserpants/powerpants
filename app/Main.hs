module Main where

import Foreign
import Foreign.C.String
import Math.Ginac.FFI
import Powerpants

main :: IO ()
main = do

    x        <- withCString "x" ginac_symbol_new
    ex_x     <- ginac_ex_new_from_basic x
    ex_1     <- ginac_ex_new_from_int 1          -- 1
    ex_2     <- ginac_ex_new_from_int 2          -- 2
    ex_neg_x <- ginac_ex_neg ex_x                -- (-x)
    ex_1_m_x <- ginac_ex_add ex_1 ex_neg_x       -- 1-x
    ex_x2    <- ginac_ex_pow ex_x ex_2           -- x^2
    ex_negx2 <- ginac_ex_neg ex_x2               -- (-x^2)
    ex1mxmx2 <- ginac_ex_add ex_1_m_x ex_negx2   -- (1-x-x^2)
    ex_frac  <- ginac_ex_div ex_x ex1mxmx2       -- x/(1-x-x^2)

    --ex_frac  <- ginac_ex_div ex_1 ex_1_m_x       -- 1/(1-x)

    fex <- xxxx ex_frac x
    --mapM_ print [truncate $ getCoeff i p q | i <- [0..800]]
    mapM_ print (baaaz fex)


    --xxy <- abab funex 0 5
    --print xxy
--    print (bazz funex)   -- GOOD
--    print (bazz2 funex) 

    --let xxy = abab funex 10 20
    --print xxy

    --serex <- yyyy funex

    --print ((zzzz serex))

    --rel      <- ginac_relation_eq_new ex_x ex_0  -- x == 0
    --series   <- ginac_ex_series ex_frac rel 100

    --(Expansion ex s n) <- yyy ex_frac

    --ex <- newForeignPtr ginac_ex_free_fun series
    --s  <- newForeignPtr ginac_basic_free_fun x

    --print (take 44 (xxx ex s))

    pure ()

--  module Main where
--  
--  import System.IO.Unsafe
--  import Powerpants
--  import Math.Ginac.FFI
--  import Foreign
--  
--  main :: IO ()
--  main = do
--  
--      let gx = X/(1 - X - X^2)
--  ----    print (expand 20 gx)
--  --
--  --    print (take 800 (expnd2 gx))
--  
--  
--  --
--  
--      print (take 902 (expnd2 gx))
--  
--  --    x <- ginac_symbol_static
--  --    p <- ginac_ex_new_from_basic x
--  --    z <- ginac_ex_new_from_int 0
--  --    r <- ginac_relation_eq_new p z
--  --    s <- withExPtr gx (\ptr -> ginac_ex_series ptr r 900)
--  --    ss <- newForeignPtr ginac_ex_free_fun s
--  --
--  --    print (take 100 (expnd3 s x))
--  
--      --ginac_ex_free s
--  
--  
--  --
--  
--  
--  
--  --    print (expnd gx 30)
--  --    print (coefficient gx 555)
--  
--      --print (take 30 $ fmap unsafePerformIO (yyy gx))
--  
--  --    print (expnd gx )
--  
--  --    let hx = X^3 * gx
--  --    print (expand 20 hx)
--  --
--  --    let fx = 2*X^2
--  --    print (expand 20 fx)
--  --
--  --    let fx' = 1/(X^2)
--  --    print (expand 20 fx')
--  --
--  --    print (nth 20 fx')
--  --    print (nth 3 gx)
--  
--  --    let ix = X^^(-3) * hx
--  --    print (expand 20 ix)
--  --    print (expand 20 ix == expand 20 gx)
--  
--  --    let jx = X^^(-6) * gx
--  --    print (expand 1 jx)
--      --
--  --
--  --    let ax = 1/(1 - X)
--  --    print (expand 20 ax)
--  --
--  --    let bx = ddx ax
--  --    print (expand 20 bx)
--  --
--  --    let cx = ax + bx
--  --    print (expand 20 cx)
--  
--      pure ()
