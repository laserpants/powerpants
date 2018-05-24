module Main where

import Powerpants

main :: IO ()
main = do
    
    -- let gx = 1/(1 - x) :: OGF
    let f = var x + 5 :: OGF

    print (substitute f 5)

    pure ()
