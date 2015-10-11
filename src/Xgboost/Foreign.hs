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

-- For the prelude of the extracted C documentation, see
-- xgboost/wrapper/xgboost_wrapper.h
-- (relative to this root of this package)

{-
/*!
 * \brief get string message of the last error
 *
 *  all function in this file will return 0 when success
 *  and -1 when an error occured,
 *  XGBGetLastError can be called to retrieve the error
 *
 *  this function is threadsafe and can be called by different thread
 * \return const char* error inforomation
 */
XGB_DLL const char *XGBGetLastError();
-}
foreign import ccall "XGBGetLastError"
  xgboostGetLastError :: CString -> IO CInt

{-
/*!
 * \brief load a data matrix
 * \param fname the name of the file
 * \param silent whether print messages during loading
 * \param out a loaded data matrix
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGDMatrixCreateFromFile(const char *fname,
                                    int silent,
                                    DMatrixHandle *out);
-}
foreign import ccall "XGDMatrixCreateFromCSR"
  xgboostMatrixCreateFromCSR :: (Ptr CULong) -> (Ptr CUInt) -> (Ptr CFloat) -> CULong -> CULong -> (Ptr DMatrixHandle) -> IO CInt

{-
/*!
 * \brief load a data matrix
 * \param fname the name of the file
 * \param silent whether print messages during loading
 * \param out a loaded data matrix
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGDMatrixCreateFromFile(const char *fname,
                                    int silent,
                                    DMatrixHandle *out);
-}
foreign import ccall "XGDMatrixCreateFromFile"
  xgboostMatrixCreateFromFile :: (Ptr CFloat) -> CInt -> CULong -> CFloat -> (Ptr DMatrixHandle) -> IO CInt

{-
/*!
 * \brief create matrix content from dense matrix
 * \param data pointer to the data space
 * \param nrow number of rows
 * \param ncol number columns
 * \param missing which value to represent missing value
 * \param out created dmatrix
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGDMatrixCreateFromMat(const float *data,
                                   bst_ulong nrow,
                                   bst_ulong ncol,
                                   float  missing,
                                   DMatrixHandle *out);
-}
foreign import ccall "XGDMatrixCreateFromMat"
  xgboostMatrixCreateFromMat :: (Ptr CFloat) -> CULong -> CULong -> CFloat -> (Ptr DMatrixHandle) -> IO CInt

{-
/*!
 * \brief create a new dmatrix from sliced content of existing matrix
 * \param handle instance of data matrix to be sliced
 * \param idxset index set
 * \param len length of index set
 * \param out a sliced new matrix
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGDMatrixSliceDMatrix(DMatrixHandle handle,
                                  const int *idxset,
                                  bst_ulong len,
                                  DMatrixHandle *out);
-}
foreign import ccall "XGDMatrixSliceDMatrix"
  xgboostMatrixSliceDMatrix :: DMatrixHandle -> (Ptr CInt) -> CULong -> (Ptr DMatrixHandle) -> IO CInt

{-
/*!
 * \brief free space in data matrix
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGDMatrixFree(void *handle);
-}
foreign import ccall "XGDMatrixFree"
  xgboostMatrixFree :: DMatrixHandle -> IO CInt

{-
/*!
 * \brief load a data matrix into binary file
 * \param handle a instance of data matrix
 * \param fname file name
 * \param silent print statistics when saving
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGDMatrixSaveBinary(DMatrixHandle handle,
                                const char *fname, int silent);
-}
foreign import ccall "XGDMatrixSaveBinary"
  xgboostMatrixSaveBinary :: DMatrixHandle -> CString -> CInt -> IO CInt

{-
/*!
 * \brief set float vector to a content in info
 * \param handle a instance of data matrix
 * \param field field name, can be label, weight
 * \param array pointer to float vector
 * \param len length of array
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGDMatrixSetFloatInfo(DMatrixHandle handle,
                                  const char *field,
                                  const float *array,
                                  bst_ulong len);
-}
foreign import ccall "XGDMatrixSetFloatInfo"
  xgboostMatrixSetFloatInfo :: DMatrixHandle -> CString -> (Ptr CFloat) -> CULong -> IO CInt

{-
/*!
 * \brief set uint32 vector to a content in info
 * \param handle a instance of data matrix
 * \param field field name
 * \param array pointer to float vector
 * \param len length of array
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGDMatrixSetUIntInfo(DMatrixHandle handle,
                                 const char *field,
                                 const unsigned *array,
                                 bst_ulong len);
-}
foreign import ccall "XGDMatrixSetUIntInfo"
  xgboostMatrixSetUIntInfo :: DMatrixHandle -> CString -> (Ptr CUInt) -> CULong -> IO CInt

{-
/*!
 * \brief set label of the training matrix
 * \param handle a instance of data matrix
 * \param group pointer to group size
 * \param len length of array
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGDMatrixSetGroup(DMatrixHandle handle,
                              const unsigned *group,
                              bst_ulong len);
-}
foreign import ccall "XGDMatrixSetGroup"
  xgboostMatrixSetGroup :: DMatrixHandle -> (Ptr CUInt) -> CULong -> IO CInt

{-
 * \brief get float info vector from matrix
 * \param handle a instance of data matrix
 * \param field field name
 * \param out_len used to set result length
 * \param out_dptr pointer to the result
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGDMatrixGetFloatInfo(const DMatrixHandle handle,
                                  const char *field,
                                  bst_ulong* out_len,
                                  const float **out_dptr);
-}
foreign import ccall "XGMMatrixGetFloatInfo"
  xgboostMatrixGetFloatInfo :: DMatrixHandle -> CString -> (Ptr CULong) -> IO (Ptr (Ptr CFloat))

{-
/*!
 * \brief get uint32 info vector from matrix
 * \param handle a instance of data matrix
 * \param field field name
 * \param out_ptr pointer to the result
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGDMatrixGetUIntInfo(const DMatrixHandle handle,
                                 const char *field,
                                 bst_ulong* out_len,
                                 const unsigned **out_dptr);
-}
foreign import ccall "XGMMatrixGetUIntInfo"
  xgboostMatrixGetUIntInfo :: DMatrixHandle -> CString -> (Ptr CULong) -> IO (Ptr (Ptr CUInt))

{-
/*!
 * \brief get number of rows
 * \param handle the handle to the DMatrix
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGDMatrixNumRow(DMatrixHandle handle,
                            bst_ulong *out);
-}
foreign import ccall "XGDMatrixNumRow"
  xgboostMatrixNumRow :: DMatrixHandle -> (Ptr CULong) -> IO CInt

{-
/*!
 * \brief get number of columns
 * \param handle the handle to the DMatrix
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGDMatrixNumCol(DMatrixHandle handle,
                            bst_ulong *out);
-}
foreign import ccall "XGDMatrixNumCol"
  xgboostMatrixNumCol :: DMatrixHandle -> (Ptr CULong) -> IO CInt

foreign import ccall "test.c test"
  test :: CInt -> IO CInt


