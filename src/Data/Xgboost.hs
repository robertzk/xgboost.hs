module Data.Xgboost (
  xgboost, Xgboost, test, test2, test3,
  
  xgboostDMatrixCreateFromMat, new, DMatrixHandle, DMH, Ptr
) where

import Data.Xgboost.Construct
import Data.Xgboost.Example

test2 :: Int -> Int
test2 = (+) 1
