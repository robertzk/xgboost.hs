{-# LANGUAGE ForeignFunctionInterface #-}
{-# CFILES test.c #-} 

module Data.Xgboost.Example (
  test
) where

import Foreign
import Foreign.C

foreign import ccall "test.c test"
  test :: CInt -> IO CInt

foreign export ccall triple :: Int -> Int
triple = (* 3)


