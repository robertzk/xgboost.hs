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

foreign import ccall "XGDMatrixCreateFromCSR"
  xgboostMatrixCreateFromFile :: (Ptr CULong) -> (Ptr CUInt) -> (Ptr CFloat) -> CULong -> CULong -> (Ptr DMatrixHandle) -> IO CInt

foreign import ccall "XGDMatrixCreateFromFile"
  xgboostMatrixCreateFromFile :: (Ptr CFloat) -> CInt -> CULong -> CFloat -> (Ptr DMatrixHandle) -> IO CInt

foreign import ccall "XGDMatrixCreateFromMat"
  xgboostMatrixCreateFromMat :: (Ptr CFloat) -> CULong -> CULong -> CFloat -> (Ptr DMatrixHandle) -> IO CInt

foreign import ccall "XGDMatrixSliceDMatrix"
  xgboostMatrixSliceDMatrix :: DMatrixHandle -> (Ptr CInt) -> CULong -> (Ptr DMatrixHandle) -> IO CInt

foreign import ccall "XGDMatrixFree"
  xgboostMatrixFree :: DMatrixHandle -> IO CInt

foreign import ccall "XGDMatrixSaveBinary"
  xgboostMatrixSaveBinary :: DMatrixHandle -> CString -> CInt -> IO CInt

foreign import ccall "XGDMatrixSetFloatInfo"
  xgboostMatrixSetFloatInfo :: DMatrixHandle -> CString -> (Ptr CFloat) -> CULong -> IO CInt

foreign import ccall "XGDMatrixSetUIntInfo"
  xgboostMatrixSetUIntInfo :: DMatrixHandle -> CString -> (Ptr CUInt) -> CULong -> IO CInt

foreign import ccall "XGDMatrixSetGroup"
  xgboostMatrixSetGroup :: DMatrixHandle -> (Ptr CUInt) -> CULong -> IO CInt

foreign import ccall "XGDMatrixNumRow"
  xgboostMatrixNumRow :: DMatrixHandle -> (Ptr CULong) -> IO CInt

foreign import ccall "XGDMatrixNumCol"
  xgboostMatrixNumCol :: DMatrixHandle -> (Ptr CULong) -> IO CInt

foreign import ccall "test.c test"
  test :: CInt -> IO CInt


