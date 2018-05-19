module Ginac.FFI where

import Foreign

data GinacEx
data GinacSymbol

type Ginac = ForeignPtr GinacEx

foreign import ccall "ginac_symbol"
    c_ginac_symbol :: IO (Ptr GinacSymbol)


