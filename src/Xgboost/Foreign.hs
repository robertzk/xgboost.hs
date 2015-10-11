-- https://wiki.haskell.org/CPlusPlus_from_Haskell
-- However, we use the C interface to Xgboost.

module Xgboost.Foreign ( 
  xgboostGetLastError,
  xgboostMatrixCreateFromMat,
  xgboostMatrixNumRow,
  xgboostMatrixNumCol
) where

import qualified Foreign
import Foreign.C
import Foreign.Ptr

type DMatrixHandle = Ptr ()

foreign import ccall "XGBGetLastError"
  xgboostGetLastError :: CString -> IO CInt

foreign import ccall "XGDMatrixCreateFromFile"
  xgboostMatrixCreateFromFile :: (Ptr CFloat) -> CInt -> CULong -> CFloat -> (Ptr DMatrixHandle) -> IO CInt

foreign import ccall "XGDMatrixCreateFromMat"
  xgboostMatrixCreateFromMat :: (Ptr CFloat) -> CULong -> CULong -> CFloat -> (Ptr DMatrixHandle) -> IO CInt

foreign import ccall "XGDMatrixNumRow"
  xgboostMatrixNumRow :: DMatrixHandle -> (Ptr CULong) -> IO CInt

foreign import ccall "XGDMatrixNumCol"
  xgboostMatrixNumCol :: DMatrixHandle -> (Ptr CULong) -> IO CInt

foreign import ccall "test.c test"
  test :: CInt -> IO CInt


