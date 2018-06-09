{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
module Powerpants.Expr.Temp where

import Algebra.Ring
import Algebra.ToInteger
import NumericPrelude
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


