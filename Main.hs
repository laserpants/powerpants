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

    -- mapM_ print (Main.take 614 ax)

    let bx = 1 / (1 - x) :: OGF
    let cx = 1 / (2 - x) :: OGF
    let dx = bx + cx

    mapM_ print (Main.take 10 bx)

    pure ()
