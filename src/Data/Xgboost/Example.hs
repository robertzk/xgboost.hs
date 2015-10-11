{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# CFILES test.c xgboost_wrapper.cpp #-} 

module Data.Xgboost.Example (
  test, test3,
  
  xgboostDMatrixCreateFromMat, new, DMatrixHandle, DMH, Ptr
) where

import qualified Foreign
import Foreign.C
import Foreign.Ptr

foreign import ccall "test.c test"
  test :: CInt -> IO CInt

foreign export ccall triple :: Int -> Int
triple = (* 3)

test3 :: Int -> Int
test3 = (* 6)

data DMatrixHandle = DMatrixHandle deriving (Show, Eq)
newtype DMH = IO (Ptr ())

class New a where
  new :: IO (Ptr a)

instance New DMatrixHandle where
  new = hnew voidPtrSize

instance New DMH where
  new = hnew voidPtrSize

instance New () where
  new = hnew voidPtrSize

hnew :: (New a) => Foreign.Word -> IO (Ptr a)
hnew size = do
  o <- cnew size
  return (castPtr o)


{-
 #include <iostream>
 int main(int argc, char** argv) { std::cout << sizeof(const float *) << " " << sizeof(unsigned long) << " " << sizeof(float); }
-}
voidPtrSize       = 8 -- TODO: (RK) Detect from system
constFloatPtrSize = 8
unsignedLongSize  = 8
floatSize         = 4

infixl 0 ->>
c ->> m = m c

-- https://wiki.haskell.org/CPlusPlus_from_Haskell
foreign import ccall "xgboost_wrapper.cpp _XGDMatrixCreateFromMat"
  xgboostDMatrixCreateFromMat :: (Ptr CFloat) -> CULong -> CULong -> CFloat -> (Ptr ()) -> IO CInt

foreign import ccall "_Znwm" cnew :: Foreign.Word -> IO (Ptr ())

