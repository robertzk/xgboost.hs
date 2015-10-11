module Xgboost (
  xgboost, Xgboost, test, test2, test3,
  
  new, MatrixHandle, DMH, Ptr, cnew
) where

import Xgboost.Construct
import Xgboost.Example

test2 :: Int -> Int
test2 = (+) 1
