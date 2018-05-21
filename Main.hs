module Main where

import Data.Maybe ( fromJust )
import Powerpants
import Powerpants.Ginac

take :: Int -> OGF -> [Integer]
take m gf = (round . fromJust) <$> rec 0 gf where
  rec n (OGF ex) = if m == n
    then []
    else Powerpants.eval (OGF (ex/factorial n)) 0 : rec (n + 1) (OGF (diff ex))

main :: IO ()
main = do
    let ax = x / (1 - x - x^2) :: OGF

    -- print (Main.take 614 ax)

    let bx = 1 / (1 - x) :: OGF
    let cx = 2 / (1 - x) :: OGF
    let dx = bx + cx

    print (Main.take 10 ax)

    print (Main.take 10 bx)
    print (Main.take 10 cx)

    print (Main.take 10 dx)

    pure ()
