module Xgboost.Foreign ( 
  xgboostDMatrixCreateFromMat,
  xgboostDMatrixNumRow,
  xgboostDMatrixNumCol
) where

import qualified Foreign
import Foreign.C
import Foreign.Ptr

-- https://wiki.haskell.org/CPlusPlus_from_Haskell
foreign import ccall "XGDMatrixCreateFromMat"
  xgboostDMatrixCreateFromMat :: (Ptr CFloat) -> CULong -> CULong -> CFloat -> (Ptr (Ptr ())) -> IO CInt

foreign import ccall "XGDMatrixNumRow"
  xgboostDMatrixNumRow :: (Ptr ()) -> (Ptr CULong) -> IO CInt

foreign import ccall "XGDMatrixNumCol"
  xgboostDMatrixNumCol :: (Ptr ()) -> (Ptr CULong) -> IO CInt

foreign import ccall "test.c test"
  test :: CInt -> IO CInt
