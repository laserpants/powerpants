import Powerpants.Expr
import Powerpants.Expr.Simplify
import Test.Hspec

--testFlattened :: SpecWith ()
--testFlattened =
--
--    describe "flattened" $
--      it "" $
--
--        ordered (flattened (Mul [Num 5, X, Mul [Num 3, Num 2], Num 2, X]))
--          `shouldBe` Mul [X,X,Num 2,Num 2,Num 3,Num 5]
--
--testFolded :: SpecWith ()
--testFolded =
--
--      it "" $ do
--
--        eval (4 :: Double) (folded (Add [Num 5, X, Mul [Num 3, Num 2], Num 2, X]))
--          `shouldBe` eval 4 (Add [Num 5, X, Mul [Num 3, Num 2], Num 2, X])
--
--        eval (7 :: Double) (folded (Add [Num 5, X, Mul [Num 3, Num 2], Num 2, X]))
--          `shouldBe` eval 7 (Add [Num 5, X, Mul [Num 3, Num 2], Num 2, X])
--
--testCollectNums :: SpecWith ()
--testCollectNums =
--
--    describe "collectConsts" $
--      it "" $ do
--
--        collectConsts [Num 4, X]
--          `shouldBe` ([4], [X])
--
--        collectConsts [Num 5, X, Mul [Num 3, Num 2], Num 2, X]
--          `shouldBe` ([5, 2], [X, Mul [Num 3, Num 2], X])

main :: IO ()
main = pure () -- hspec $ do
--    testFlattened
--    testFolded
--    testCollectNums
