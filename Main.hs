module Main where

import Powerpants
import Powerpants.Ginac

main :: IO ()
main = do
--    let ax = GF $ 1 / (1 - x)
--    let bx = GF $ 1 / (1 - x)
--    let cx = ax + bx
--    printGF cx
--    let b = num 8
--    let c = Powerpants.Ginac.signum b
--    printEx c
    let xx = 1 / (1 - x) :: Expr
    let yy = eval xx 0
    printEx yy
    pure ()
