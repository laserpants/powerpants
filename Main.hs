module Main where

import Powerpants
import Powerpants.Ginac

baz = f 0 where
  f n e = if 400 == n
          then []
          else eval (e / factorial n) 0 : f (n + 1) (diff e)

main :: IO ()
main = do
    let xs = baz (1/(1-x))
    mapM_ printEx xs
  
--    let bx = GF $ 1 / (1 - x)
--    let cx = ax + bx
--    printGF cx
--    let b = num 8
--    let c = Powerpants.Ginac.signum b
--    printEx c
    let xx = 1 / (1 - x) :: Expr
    let yy = eval xx 0
    let zz = diff xx
    printEx zz
    pure ()
