import Test.QuickCheck

-- Właściwości dla operacji arytmetycznych
prop_plusAssociativeInt :: Int -> Int -> Int -> Bool
prop_plusAssociativeInt x y z = x + (y + z) == (x + y) + z

prop_plusAssociativeDouble :: Double -> Double -> Double -> Bool
prop_plusAssociativeDouble x y z = x + (y + z) == (x + y) + z

prop_plusCommutativeInt :: Int -> Int -> Bool
prop_plusCommutativeInt x y = x + y == y + x

prop_plusCommutativeDouble :: Double -> Double -> Bool
prop_plusCommutativeDouble x y = x + y == y + x

-- Właściwość dla funkcji reverse
prop_reverseTrue :: [Int] -> Bool
prop_reverseTrue x = reverse [x] == [x]

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

-- Nowe właściwości dla list
prop_reverseAppend1 :: [Int] -> [Int] -> Bool
prop_reverseAppend1 xs ys = reverse (xs ++ ys) == reverse xs ++ reverse ys

prop_reverseAppend2 :: [Int] -> [Int] -> Bool
prop_reverseAppend2 xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

-- Właściwość dla funkcji połowienia
prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = ((* 2) . (/ 2) $ x) == x

-- Główna funkcja uruchamiająca testy
main :: IO ()
main = do
  putStrLn "\n*** Testing prop_plusAssociativeInt... ***"
  verboseCheck (withMaxSuccess 1000 prop_plusAssociativeInt)
  
  putStrLn "\n*** Testing prop_plusAssociativeDouble... ***"
  verboseCheck (withMaxSuccess 1000 prop_plusAssociativeDouble)
  
  putStrLn "\n*** Testing prop_plusCommutativeInt... ***"
  verboseCheck (withMaxSuccess 1000 prop_plusCommutativeInt)
  
  putStrLn "\n*** Testing prop_plusCommutativeDouble... ***"
  verboseCheck (withMaxSuccess 1000 prop_plusCommutativeDouble)
  
  putStrLn "\n*** Testing prop_reverseTrue... ***"
  verboseCheck (withMaxSuccess 1000 prop_reverseTrue)
  
  putStrLn "\n*** Testing prop_reverse... ***"
  verboseCheck (withMaxSuccess 500 prop_reverse)
  
  putStrLn "\n*** Testing prop_reverseAppend1... ***"
  verboseCheck (withMaxSuccess 500 prop_reverseAppend1)
  
  putStrLn "\n*** Testing prop_reverseAppend2... ***"
  verboseCheck (withMaxSuccess 500 prop_reverseAppend2)
  
  putStrLn "\n*** Testing prop_halfIdentity... ***"
  verboseCheck prop_halfIdentity
