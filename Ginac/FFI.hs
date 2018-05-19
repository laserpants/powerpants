module Ginac.FFI where

import Foreign

data GinacExpr

type Ginac = ForeignPtr GinacExpr
