import Test.QuickCheck

isOrdered :: Ord a => [a] -> Bool
isOrdered [] = True
isOrdered xs = and $ zipWith (<=) xs (tail xs)

insertToOrdList :: Ord a => a -> [a] -> [a]
insertToOrdList e xs = takeWhile (< e) xs ++ [e] ++ dropWhile (< e) xs

-- Właściwości testowe
prop_MaxOfX_LtOrEq_YIsY :: Int -> Int -> Property
prop_MaxOfX_LtOrEq_YIsY x y = x <= y ==> max x y == y

prop_InsertPreservesOrder :: Int -> [Int] -> Property
prop_InsertPreservesOrder x xs = isOrdered xs ==> isOrdered (insertToOrdList x xs)

prop_InsertPreservesOrder3 :: Int -> [Int] -> Property
prop_InsertPreservesOrder3 x xs =
  isOrdered xs ==> collect (length xs) $
    isOrdered (insertToOrdList x xs)

-- prop_DoubleCycleEqOneCycle :: [Int] -> Property
-- prop_DoubleCycleEqOneCycle xs = not (null xs) ==> cycle xs == cycle (xs ++ xs)

zadanie :: Int -> Int -> Property
zadanie x y = x <= y ==> min x y == x

prop_InsertPreservesOrder4 :: Int -> [Int] -> Property
prop_InsertPreservesOrder4 x xs =
  forAll orderedList $ \xs -> isOrdered (insertToOrdList x xs)

main :: IO ()
main = do
  putStrLn "\n*** Testing prop_MaxOfX_LtOrEq_YIsY... ***"
  quickCheck prop_MaxOfX_LtOrEq_YIsY
  putStrLn "\n*** Testing prop_InsertPreservesOrder... ***"
  quickCheck prop_InsertPreservesOrder
  putStrLn "\n*** Testing prop_InsertPreservesOrder3... ***"
  quickCheck prop_InsertPreservesOrder3
  -- putStrLn "\n*** Testing prop_DoubleCycleEqOneCycle... ***"
  -- quickCheck prop_DoubleCycleEqOneCycle
  putStrLn "\n*** Testing zadanie... ***"
  quickCheck zadanie
  putStrLn "\n*** Testing kolejne... ***"
  quickCheck prop_InsertPreservesOrder4
