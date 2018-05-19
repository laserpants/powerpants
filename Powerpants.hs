module Powerpants where

import Ginac.FFI

data Expr = Expr Ginac | X

data GF = GF Expr
