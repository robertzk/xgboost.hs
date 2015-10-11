{-# LANGUAGE DataKinds #-} 

module Xgboost.Construct (
  xgboost, Xgboost
) where

import qualified Data.Vector as V

xgboost :: Num a => a
xgboost = 1

data Xgboost a = M {
   nrow :: Int
 , ncol :: Int
 , vect :: V.Vector a
}




