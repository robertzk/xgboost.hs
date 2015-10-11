module Xgboost (
  xgboost, Xgboost, test, test2, test3,
  
  new, DMatrixHandle, DMH, Ptr, cnew,

  -- Foreign
  xgboostDMatrixCreateFromMat,
  xgboostDMatrixNumRow,
  xgboostDMatrixNumCol
) where

import Xgboost.Construct
import Xgboost.Example
import Xgboost.Foreign

test2 :: Int -> Int
test2 = (+) 1
