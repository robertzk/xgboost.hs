module Xgboost (
  xgboost, Xgboost, test, test2, test3,
  
  xgboostDMatrixCreateFromMat, new, DMatrixHandle, DMH, Ptr, cnew,
  xgboostDMatrixNumRow, xgboostDMatrixNumCol
) where

import Xgboost.Construct
import Xgboost.Example

test2 :: Int -> Int
test2 = (+) 1
