main :: IO ()
main = do
  check_trivial

-- Stolen: https://github.com/haskell/pretty/blob/master/tests/Test.hs
myAssert :: String -> Bool -> IO ()
myAssert msg b = putStrLn $ (if b then "Ok, passed " else "Failed test:\n  ") ++ msg

check_trivial = do
  putStrLn " = Trivial Tests = "
  myAssert "Trivial test" True

