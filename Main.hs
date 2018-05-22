module Main where

import Data.Maybe ( fromJust, fromMaybe )
import Powerpants
import Powerpants.Ginac

import qualified Powerpants as GF

list :: OGF -> [Integer]
list gf = maybe 0 round <$> rec 0 gf where
  rec n ex = GF.eval (ex / Gx (factorial n)) 0 : rec (n + 1) (GF.diff ex)

--take :: Int -> OGF -> [Integer]
--take m gf = (round . fromJust) <$> rec 0 gf where
--  rec n ex = 
--    let f = Gx (factorial n)
--     in if m == n 
--          then []
--          else GF.eval (ex / f) 0 : rec (n + 1) (GF.diff ex)

--toList :: OGF -> [Integer]
--toList gf = [cx gf i | i <- [0..100]]

main :: IO ()
main = do
    let ax = x/(1-x-x^2)

    print (GF.toString ax)

    print (list ax !! 600)

    --let str = toString ax 

    --putStrLn str

--    -- print (Main.take 614 ax)
--
--    let bx = Gx $ 1 / (1 - x)
--    let cx = Gx $ 2 / (1 - x)
--    let dx = bx + cx

    --putStrLn (GF.toString ax)

--    print (toList ax !! )
--    print (Main.take 533 ax)

--    print (Main.take 133 bx)
--    print (Main.take 133 cx)
--
--    print (Main.take 133 dx)

    pure ()
