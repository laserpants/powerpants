module Main where

import Powerpants
import Ginac

main :: IO ()
main = do
    let ax = GF (1 + x) -- GF $ 1 / (1 - x)
    let bx = GF (2 + x) -- $ 1 / (1 - x)
    let cx = ax + bx
    printGF cx
    let b = num (-5)
    let c = Ginac.abs b
    printEx c
    pure ()
