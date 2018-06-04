import Powerpants.Expr
import Powerpants.Expr.Simplify
import Test.Hspec

testDivnode :: SpecWith ()
testDivnode =

    describe "divnode" $
      it "eliminates intermediate division nodes" $ do
        divnode (Div (Div (Num 1) (Num 2)) (Num 3)) `shouldBe` Div (Num 1) (Mul [Num 2, Num 3])
        divnode (Div (Num 1) (Div (Num 2) (Num 3))) `shouldBe` Div (Mul [Num 1, Num 3]) (Num 2)

main :: IO ()
main = hspec testDivnode
