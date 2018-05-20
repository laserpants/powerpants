module Ginac.FFI where

import Foreign

data GinacEx
data GinacSymbol

type Ginac = ForeignPtr GinacEx

foreign import ccall "ginac_symbol"
    ginac_symbol :: IO (Ptr GinacSymbol)

foreign import ccall "ginac_ex_new_symbol"
    ginac_ex_new_symbol :: Ptr GinacSymbol -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_new_x"
    ginac_ex_new_x :: IO (Ptr GinacEx)

foreign import ccall "ginac_ex_free"
    ginac_ex_free :: Ptr GinacEx -> IO ()

foreign import ccall "&ginac_ex_free"
    ginac_ex_free_fun :: FunPtr (Ptr GinacEx -> IO ())
