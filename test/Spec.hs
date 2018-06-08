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

-- Add [ Mul [ Num 0, Mul [ Pow ( Add [ Num 1, Mul [ Num (-1), X ] ]) (-1) ] ], Mul [ Num 1, Mul [ Num (-1), Pow ( Add [ Num 1, Mul [ Num (-1), X ] ]) (-2), Add [ Num 0, Add [ Mul [ Num 0, Mul [X] ], Mul [ Num (-1), Num 1 ] ] ] ] ] ]
expr_0 =
  Add [
    Mul [
      Num 0,
      Mul [
        Pow (
          Add [
            Num 1,
            Mul [
              Num (-1),
              X
            ]
          ]) (-1)
      ]
    ],
    Mul [
      Num 1,
      Mul [
        Num (-1),
        Pow (
          Add [
            Num 1,
            Mul [
              Num (-1),
              X
            ]
          ]) (-2),
        Add [
          Num 0,
          Add [
            Mul [
              Num 0,
              Mul [X]
            ],
            Mul [
              Num (-1),
              Num 1
            ]
          ]
        ]
      ]
    ]
  ]

test1 :: SpecWith ()
test1 = do

    describe "combined" $
      it "shouldn't change the meaning of the expression" $

        eval 3 expr_0 `shouldBe` eval 3 (combined expr_0)

    describe "compressed" $
      it "shouldn't change the meaning of the expression" $

        eval 3 expr_0 `shouldBe` eval 3 (compressed expr_0)

    describe "flattened" $
      it "shouldn't change the meaning of the expression" $

        eval 3 expr_0 `shouldBe` eval 3 (flattened expr_0)

main :: IO ()
main = hspec
    test1
--    testFlattened
--    testFolded
--    testCollectNums
