module Main where

import Powerpants

main :: IO ()
main = do

    let gx = X/(1 - X - X^2)
    print (expandedTo 20 gx)

    let fx = lsh 2 gx
    print (expandedTo 20 fx)

    -- let fx = rsh 2 gx
    -- print (expandedTo 20 fx)

    -- let ex = lsh 2 fx
    -- print (expandedTo 20 ex)
    -- print (expandedTo 20 ex == expandedTo 20 gx)

--    let ax = 1/(1 - X)
--    print (expandedTo 20 ax)
--
--    let bx = ddx ax
--    print (expandedTo 20 bx)
--
--    let cx = ax + bx
--    print (expandedTo 20 cx)

    pure ()
