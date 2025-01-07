fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

fstDivSec :: Integral a => [a] -> Bool
fstDivSec (x : y : _) | y `mod` x == 0 = True
fstDivSec _                        = False

fstDivThird :: Integral a => [a] -> Bool
fstDivThird (x : y : z : _) | z `mod` x == 0 = True
fstDivThird _ = False

absAll :: Num t => [t] -> [t]
absAll [] = []
absAll (x:xs) = abs x : absAll xs

sumAll :: Num a => [a] -> a
sumAll = loop 0
    where loop acc [] = acc
          loop acc (x:xs) = loop (x + acc) xs


