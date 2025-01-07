sumWith g [] = 0
sumWith g (x:xs) = g x + sumWith g xs

prodWith g [] = 1
prodWith g (x:xs) = g x * prodWith g xs

sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' = go 0
  where
    go acc g [] = acc
    go acc g (x:xs) = go (g x + acc) g xs


foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

sumWith'' :: Num a => (a -> a) -> [a] -> a
sumWith'' g = foldr' (\x acc -> g x + acc) 0

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x:xs) = foldl' f (f z x) xs

sumWith''' :: Num a => (a -> a) -> [a] -> a
sumWith''' g = foldl' (\acc x -> acc + g x) 0