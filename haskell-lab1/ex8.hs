not' :: Bool -> Bool
not' b = case b of
    True -> False
    False -> True

absInt n =
    case (n >= 0) of
        True -> 1
        _ -> -n


isItTheAnswer :: String -> Bool
isItTheAnswer s = case s of
    "Love" -> True
    _ -> False

or' :: (Bool, Bool) -> Bool
or' (a, b) = case (a, b) of
    (True, _) -> True
    (_, True) -> True
    _         -> False

and' :: (Bool, Bool) -> Bool
and' (a, b) = case (a, b) of
    (True, True) -> True
    _            -> False

collatz :: Int -> Int
collatz n = let divides d n = n `mod` d == 0
    isEven n = divides(2,n)
in if isEven n then n `div` 2
   else 3 * n + 1