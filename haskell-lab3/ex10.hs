isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = all (uncurry (<=)) (zip xs (tail xs))

everySecond :: [t] -> [t]
everySecond xs = [x | (x, i) <- zip xs [0..], even i]
