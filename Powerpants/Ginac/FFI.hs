module Powerpants.Ginac.FFI where

import Foreign

data GinacEx
data GinacSymbol

type GinacExPtr = ForeignPtr GinacEx

foreign import ccall "ginac_symbol"
    ginac_symbol :: IO (Ptr GinacSymbol)

foreign import ccall "ginac_ex_new_int"
    ginac_ex_new_int :: Int -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_new_symbol"
    ginac_ex_new_symbol :: Ptr GinacSymbol -> IO (Ptr GinacEx)

foreign import ccall "ginac_ex_new_x"
    ginac_ex_new_x :: IO (Ptr GinacEx)

foreign import ccall "ginac_ex_free"
    ginac_ex_free :: Ptr GinacEx -> IO ()

foreign import ccall "&ginac_ex_free"
    ginac_ex_free_fun :: FunPtr (Ptr GinacEx -> IO ())

foreign import ccall "ginac_ex_print"
    ginac_ex_print :: Ptr GinacEx -> IO ()

foreign import ccall "ginac_add"
    ginac_add :: Ptr GinacEx -> Ptr GinacEx -> IO (Ptr GinacEx)

foreign import ccall "ginac_abs"
    ginac_abs :: Ptr GinacEx -> IO (Ptr GinacEx)

foreign import ccall "ginac_signum"
    ginac_signum :: Ptr GinacEx -> IO (Ptr GinacEx)
