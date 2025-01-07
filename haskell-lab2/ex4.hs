import Data.Char (toUpper)

isPalindrome :: [Char] -> Bool
isPalindrome s = s == reverse s

getElemAtIdx :: [a] -> Int -> a
getElemAtIdx xs n = head (drop n xs)

capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize w = toUpper (head w) : tail w 
