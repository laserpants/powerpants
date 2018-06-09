{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Powerpants.Expr.Temp where

import Algebra.Ring
import Algebra.ToInteger
import NumericPrelude
import Powerpants.Expr.Symbolic
import Powerpants.Expr.Derivative
import Powerpants.Expr

expr0 :: Expr Integer
-- Add [ Mul [ Num 0, Mul [ Pow ( Add [ Num 1, Mul [ Num (-1), X ] ]) (-1) ] ], Mul [ Num 1, Mul [ Num (-1), Pow ( Add [ Num 1, Mul [ Num (-1), X ] ]) (-2), Add [ Num 0, Add [ Mul [ Num 0, Mul [X] ], Mul [ Num (-1), Num 1 ] ] ] ] ] ]
expr0 =
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

expr1 :: Expr Integer
expr1 = 1/(1-X)

expr2 :: Expr Integer
expr2 = 5*X^3 + 4*X^2 + 1/(1-X) + 40*X/(1-X-X^2)

expr3 :: Expr Integer
expr3 = 1/(1-X-X^2)

expr4 :: Expr Integer
expr4 = ddx (ddx (1/(1-X-X^2)))
