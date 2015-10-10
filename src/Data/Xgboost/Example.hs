module Data.Xgboost.Example (
  test
) where

import Foreign
import Foreign.C

foreign import ccall "test.c test"
  test :: CInt -> IO CInt


