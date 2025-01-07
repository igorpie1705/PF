isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []

primes :: [Int]
primes = eratoSieve [2..]
 where
   eratoSieve :: [Int] -> [Int]
   eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

isPrime2 :: Int -> Bool
isPrime2 n
  | n < 2     = False
  | otherwise = null [p | p <- takeWhile (\p -> p * p <= n) primes, n `mod` p == 0]

f :: Int -> Int
f n = length [x | x <- [1..n], isPrime2 x]

allEqual :: Eq a => [a] -> Bool
allEqual xs = all (==(head xs)) xs


