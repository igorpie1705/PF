sqr x = x ^ 2

funcFactory n = case n of
    1 -> id
    2 -> sqr
    3 -> (^3)
    4 -> \x -> x^4
    5 -> intFunc
    _ -> const n
    where
        intFunc x = x^5


-- mały problem z tym zadaniem poniżej, czy da się bez podawania x?

expApproxUpTo :: Int -> Double -> Double
expApproxUpTo n x = sum [x^k / fromIntegral (fact k) | k <- [0..n]]
  where
    fact :: Int -> Int
    fact 0 = 1
    fact k = k * fact (k - 1)


dfr :: (Double -> Double) -> Double -> (Double -> Double)
dfr f h = \x -> (f (x + h) - (f x)) / h



