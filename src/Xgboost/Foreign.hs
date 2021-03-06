{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# CFILES xgboost_wrapper.cpp #-} 

-- https://wiki.haskell.org/CPlusPlus_from_Haskell
-- However, we use the C interface to Xgboost.

module Xgboost.Foreign ( 
  
  -- Error handling
  xgboostGetLastError,

  -- Data I/O
  xgboostMatrixCreateFromCSR,
  xgboostMatrixCreateFromFile,
  xgboostMatrixCreateFromMat,
  xgboostMatrixSliceDMatrix,
  xgboostMatrixFree,
  xgboostMatrixSaveBinary,

  -- Mutators for matrix meta-data
  xgboostMatrixSetFloatInfo,
  xgboostMatrixSetUIntInfo,
  xgboostMatrixSetGroup,

  -- Accessors for matrix meta-data
  xgboostMatrixGetFloatInfo,
  xgboostMatrixGetUIntInfo,
  xgboostMatrixNumRow,
  xgboostMatrixNumCol,

  -- Booster object construction and manipulation
  xgboostBoosterCreate,
  xgboostBoosterFree,
  xgboostBoosterSetParam, 

  -- Booster training methods
  xgboostBoosterUpdateOneIter,
  xgboostBoosterBoostOneIter,
  xgboostBoosterEvalOneIter,

  -- Booster predict methods
  xgboostBoosterPredict,
  
  -- Booster object I/O
  xgboostBoosterLoadModel, 
  xgboostBoosterSaveModel, 
  xgboostBoosterLoadModelFromBuffer, 
  xgboostBoosterGetModelRaw, 
  xgboostBoosterDumpModel, 
  xgboostBoosterDumpModelWithFeatures, 

) where

import qualified Foreign
import Foreign.C
import Foreign.Ptr

type DMatrixHandle = Ptr ()
type BoosterHandle = Ptr ()
type FloatArray    = Ptr CFloat
type ModelDump     = Ptr CString

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
foreign import ccall unsafe "xgboost_wrapper.h XGBGetLastError"
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
foreign import ccall unsafe "xgboost_wrapper.h XGDMatrixCreateFromCSR"
  xgboostMatrixCreateFromCSR :: (Ptr CULong) -> (Ptr CUInt) -> FloatArray -> CULong -> CULong -> (Ptr DMatrixHandle) -> IO CInt

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
foreign import ccall unsafe "xgboost_wrapper.h XGDMatrixCreateFromFile"
  xgboostMatrixCreateFromFile :: FloatArray -> CInt -> CULong -> CFloat -> (Ptr DMatrixHandle) -> IO CInt

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
foreign import ccall unsafe "xgboost_wrapper.h XGDMatrixCreateFromMat"
  xgboostMatrixCreateFromMat :: FloatArray -> CULong -> CULong -> CFloat -> (Ptr DMatrixHandle) -> IO CInt

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
foreign import ccall unsafe "xgboost_wrapper.h XGDMatrixSliceDMatrix"
  xgboostMatrixSliceDMatrix :: DMatrixHandle -> (Ptr CInt) -> CULong -> (Ptr DMatrixHandle) -> IO CInt

{-
/*!
 * \brief free space in data matrix
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGDMatrixFree(void *handle);
-}
foreign import ccall unsafe "xgboost_wrapper.h XGDMatrixFree"
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
foreign import ccall unsafe "xgboost_wrapper.h XGDMatrixSaveBinary"
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
foreign import ccall unsafe "xgboost_wrapper.h XGDMatrixSetFloatInfo"
  xgboostMatrixSetFloatInfo :: DMatrixHandle -> CString -> FloatArray -> CULong -> IO CInt

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
foreign import ccall unsafe "xgboost_wrapper.h XGDMatrixSetUIntInfo"
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
foreign import ccall unsafe "xgboost_wrapper.h XGDMatrixSetGroup"
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
foreign import ccall unsafe "xgboost_wrapper.h XGDMatrixGetFloatInfo"
  xgboostMatrixGetFloatInfo :: DMatrixHandle -> CString -> (Ptr CULong) -> IO (Ptr FloatArray)

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
foreign import ccall unsafe "xgboost_wrapper.h XGDMatrixGetUIntInfo"
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
foreign import ccall unsafe "xgboost_wrapper.h XGDMatrixNumRow"
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
foreign import ccall unsafe "xgboost_wrapper.h XGDMatrixNumCol"
  xgboostMatrixNumCol :: DMatrixHandle -> (Ptr CULong) -> IO CInt

{-
/*!
 * \brief create xgboost learner
 * \param dmats matrices that are set to be cached
 * \param len length of dmats
 * \param out handle to the result booster
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGBoosterCreate(void* dmats[],
                            bst_ulong len,
                            BoosterHandle *out);
-}
foreign import ccall unsafe "xgboost_wrapper.h XGBoosterCreate"
  xgboostBoosterCreate :: (Ptr DMatrixHandle) -> CULong -> (Ptr BoosterHandle) -> IO CInt

{-
/*!
 * \brief free obj in handle
 * \param handle handle to be freed
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGBoosterFree(BoosterHandle handle);
-}
foreign import ccall unsafe "xgboost_wrapper.h XGBoosterFree"
  xgboostBoosterFree :: BoosterHandle -> IO CInt

{-
/*!
 * \brief set parameters
 * \param handle handle
 * \param name  parameter name
 * \param val value of parameter
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGBoosterSetParam(BoosterHandle handle,
                              const char *name,
                              const char *value);
-}
foreign import ccall unsafe "xgboost_wrapper.h XGBoosterSetParam"
  xgboostBoosterSetParam :: BoosterHandle -> CString -> CString -> IO CInt

{-
/*!
 * \brief update the model in one round using dtrain
 * \param handle handle
 * \param iter current iteration rounds
 * \param dtrain training data
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGBoosterUpdateOneIter(BoosterHandle handle,
                                   int iter,
                                   DMatrixHandle dtrain);
-}
foreign import ccall unsafe "xgboost_wrapper.h XGBoosterUpdateOneIter"
  xgboostBoosterUpdateOneIter :: BoosterHandle -> CInt -> DMatrixHandle -> IO CInt

{-
/*!
 * \brief update the model, by directly specify gradient and second order gradient,
 *        this can be used to replace UpdateOneIter, to support customized loss function
 * \param handle handle
 * \param dtrain training data
 * \param grad gradient statistics
 * \param hess second order gradient statistics
 * \param len length of grad/hess array
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGBoosterBoostOneIter(BoosterHandle handle,
                                  DMatrixHandle dtrain,
                                  float *grad,
                                  float *hess,
                                  bst_ulong len);
-}
foreign import ccall unsafe "xgboost_wrapper.h XGBoosterBoostOneIter"
  xgboostBoosterBoostOneIter :: BoosterHandle -> DMatrixHandle -> FloatArray -> FloatArray  -> CULong -> IO CInt

{-
/*!
 * \brief get evaluation statistics for xgboost
 * \param handle handle
 * \param iter current iteration rounds
 * \param dmats pointers to data to be evaluated
 * \param evnames pointers to names of each data
 * \param len length of dmats
 * \param out_result the string containing evaluation statistics
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGBoosterEvalOneIter(BoosterHandle handle,
                                 int iter,
                                 DMatrixHandle dmats[],
                                 const char *evnames[],
                                 bst_ulong len,
                                 const char **out_result);
-}
foreign import ccall unsafe "xgboost_wrapper.h XGBoosterEvalOneIter"
  xgboostBoosterEvalOneIter :: BoosterHandle -> CInt -> DMatrixHandle -> (Ptr CString) -> CULong -> (Ptr CString) -> IO CInt

{-
/*!
 * \brief make prediction based on dmat
 * \param handle handle
 * \param dmat data matrix
 * \param option_mask bit-mask of options taken in prediction, possible values
 *          0:normal prediction
 *          1:output margin instead of transformed value
 *          2:output leaf index of trees instead of leaf value, note leaf index is unique per tree
 * \param ntree_limit limit number of trees used for prediction, this is only valid for boosted trees
 *    when the parameter is set to 0, we will use all the trees
 * \param out_len used to store length of returning result
 * \param out_result used to set a pointer to array
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGBoosterPredict(BoosterHandle handle,
                             DMatrixHandle dmat,
                             int option_mask,
                             unsigned ntree_limit,
                             bst_ulong *out_len,
                             const float **out_result);
-}
foreign import ccall unsafe "xgboost_wrapper.h XGBoosterPredict"
  xgboostBoosterPredict :: BoosterHandle -> DMatrixHandle -> CInt -> CUInt -> (Ptr FloatArray) -> IO CInt

{-
/*!
 * \brief load model from existing file
 * \param handle handle
 * \param fname file name
* \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGBoosterLoadModel(BoosterHandle handle,
                               const char *fname);
-}
foreign import ccall unsafe "xgboost_wrapper.h XGBoosterLoadModel"
  xgboostBoosterLoadModel :: BoosterHandle -> CString -> IO CInt

{-
/*!
 * \brief save model into existing file
 * \param handle handle
 * \param fname file name
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGBoosterSaveModel(BoosterHandle handle,
                               const char *fname);
-}
foreign import ccall unsafe "xgboost_wrapper.h XGBoosterSaveModel"
  xgboostBoosterSaveModel :: BoosterHandle -> CString -> IO CInt

{-
/*!
 * \brief load model from in memory buffer
 * \param handle handle
 * \param buf pointer to the buffer
 * \param len the length of the buffer
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGBoosterLoadModelFromBuffer(BoosterHandle handle,
                                         const void *buf,
                                         bst_ulong len);
-}
foreign import ccall unsafe "xgboost_wrapper.h XGBoosterLoadModelFromBuffer"
  xgboostBoosterLoadModelFromBuffer :: BoosterHandle -> Ptr () -> CULong -> IO CInt

{-
/*!
 * \brief save model into binary raw bytes, return header of the array
 * user must copy the result out, before next xgboost call
 * \param handle handle
 * \param out_len the argument to hold the output length
 * \param out_dptr the argument to hold the output data pointer
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGBoosterGetModelRaw(BoosterHandle handle,
                                 bst_ulong *out_len,
                                 const char **out_dptr);
-}
foreign import ccall unsafe "xgboost_wrapper.h XGBoosterGetModelRaw"
  xgboostBoosterGetModelRaw :: BoosterHandle -> CULong -> (Ptr CString) -> IO CInt

{-
/*!
 * \brief dump model, return array of strings representing model dump
 * \param handle handle
 * \param fmap  name to fmap can be empty string
 * \param with_stats whether to dump with statistics
 * \param out_len length of output array
 * \param out_dump_array pointer to hold representing dump of each model
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGBoosterDumpModel(BoosterHandle handle,
                               const char *fmap,
                               int with_stats,
                               bst_ulong *out_len,
                               const char ***out_dump_array);
-}
foreign import ccall unsafe "xgboost_wrapper.h XGBoosterDumpModel"
  xgboostBoosterDumpModel :: BoosterHandle -> CString -> CInt -> (Ptr CULong) -> (Ptr ModelDump) -> IO CInt

{-
/*!
 * \brief dump model, return array of strings representing model dump
 * \param handle handle
 * \param fnum number of features
 * \param fnum names of features
 * \param fnum types of features
 * \param with_stats whether to dump with statistics
 * \param out_len length of output array
 * \param out_dump_array pointer to hold representing dump of each model
 * \return 0 when success, -1 when failure happens
 */
XGB_DLL int XGBoosterDumpModelWithFeatures(BoosterHandle handle,
                                           int fnum,
                                           const char **fname,
                                           const char **ftype,
                                           int with_stats,
                                           bst_ulong *len,
                                           const char ***out_models);
-}
foreign import ccall unsafe "xgboost_wrapper.h XGBoosterDumpModelWithFeatures"
  xgboostBoosterDumpModelWithFeatures :: BoosterHandle -> CInt -> (Ptr CString) -> (Ptr CString) -> CInt -> (Ptr CULong) -> (Ptr ModelDump) -> IO CInt


