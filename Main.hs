module Main where

import Powerpants

main :: IO ()
main = do
    let ax = GF (1 + x) -- GF $ 1 / (1 - x)
    let bx = GF (2 + x) -- $ 1 / (1 - x)
    let cx = ax + bx
    printGF cx
    pure ()
