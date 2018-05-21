module Main where

import Powerpants
import Powerpants.Ginac

baz = f 0 where
  f n (GF ex) = if 614 == n
          then []
          else eval (ex / factorial n) 0 : f (n + 1) (GF (diff ex))

main :: IO ()
main = do
--    let xs = baz (1/(1-x))
--    mapM_ printEx xs
--  
--    print "--------"
--
--    let bx = GF $ 1 / (1 - x)
--    let cx = ax + bx
--    printGF cx
--    let b = num 8
--    let c = Powerpants.Ginac.signum b
--    printEx c
--    let xx = 1 / (1 - x) :: Expr
--    let yy = eval xx 0
--    let zz = diff xx
--    printEx zz
--

    let ax = x / (1 - x - x^2) :: GF

    mapM_ printEx (baz ax)

    pure ()
