{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# CFILES test.c xgboost_wrapper.cpp #-} 

import Xgboost
import Xgboost.Foreign
import System.IO.Unsafe
import Foreign.C
import Foreign.Storable
import Foreign.Ptr

main :: IO ()
main = do
  check_trivial
  --check_xgboost_read

-- Stolen: https://github.com/haskell/pretty/blob/master/tests/Test.hs
myAssert :: String -> Bool -> IO ()
myAssert msg b = putStrLn $ (if b then "Ok, passed " else "Failed test:\n  ") ++ msg

check_trivial = do
  putStrLn " = Trivial Tests = "
  myAssert "Trivial test" True
--
--check_xgboost_read = do
--  let dat = unsafePerformIO $ (new :: IO (Ptr CFloat))
--  let nrow = 1 :: CULong
--  let missing = 0.5 :: CFloat
--  let dmh = unsafePerformIO $ (new :: IO (Ptr (Ptr ())))
--  let mat = xgboostMatrixCreateFromMat dat nrow nrow missing dmh
--  mat
--  let r  = unsafePerformIO $ (new :: IO (Ptr CULong))
--  let rs = xgboostMatrixNumRow (unsafePerformIO $ peek dmh) r
--  rs
--  myAssert "Number of rows is 1" $ (==) (1 :: CULong) $ unsafePerformIO $ peek r

