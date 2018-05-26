module Main where

import System.IO.Unsafe
import Powerpants
import Math.Ginac.FFI
import Foreign

main :: IO ()
main = do

    let gx = X/(1 - X - X^2)
----    print (expand 20 gx)
--
--    print (take 800 (expnd2 gx))


--

    print (take 902 (expnd2 gx))

--    x <- ginac_symbol_static
--    p <- ginac_ex_new_from_basic x
--    z <- ginac_ex_new_from_int 0
--    r <- ginac_relation_eq_new p z
--    s <- withExPtr gx (\ptr -> ginac_ex_series ptr r 900)
--    ss <- newForeignPtr ginac_ex_free_fun s
--
--    print (take 100 (expnd3 s x))

    --ginac_ex_free s


--



--    print (expnd gx 30)
--    print (coefficient gx 555)

    --print (take 30 $ fmap unsafePerformIO (yyy gx))

--    print (expnd gx )

--    let hx = X^3 * gx
--    print (expand 20 hx)
--
--    let fx = 2*X^2
--    print (expand 20 fx)
--
--    let fx' = 1/(X^2)
--    print (expand 20 fx')
--
--    print (nth 20 fx')
--    print (nth 3 gx)

--    let ix = X^^(-3) * hx
--    print (expand 20 ix)
--    print (expand 20 ix == expand 20 gx)

--    let jx = X^^(-6) * gx
--    print (expand 1 jx)
    --
--
--    let ax = 1/(1 - X)
--    print (expand 20 ax)
--
--    let bx = ddx ax
--    print (expand 20 bx)
--
--    let cx = ax + bx
--    print (expand 20 cx)

    pure ()
