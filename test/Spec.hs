import Powerpants.Expr
import Powerpants.Expr.Simplify
import Test.Hspec

testCollectNums :: SpecWith ()
testCollectNums =

    describe "collectNums" $
      it "" $ do
        collectNums [Num 4, X] `shouldBe` ([4], [X])
        collectNums [Num 5, X, Mul [Num 3, Num 2], Num 2, X] `shouldBe` ([5, 2], [X, Mul [Num 3, Num 2], X])

testDivnode :: SpecWith ()
testDivnode =

    describe "divnode" $
      it "eliminates intermediate division nodes" $ do
        divnode (Div (Div (Num 1) (Num 2)) (Num 3)) `shouldBe` Div (Num 1) (Mul [Num 2, Num 3])
        divnode (Div (Num 1) (Div (Num 2) (Num 3))) `shouldBe` Div (Mul [Num 1, Num 3]) (Num 2)

main :: IO ()
main = hspec testDivnode
