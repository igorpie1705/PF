sgn :: Int -> Int
sgn n = if n < 0
    then -1
    else if n == 0
        then 0
        else 1

absInt :: Int -> Int
absInt n = if n < 0
    then -1 * n
    else n

min2Int :: (Int, Int) -> Int
min2Int (a, b) = if (a < b)
    then a
    else b

min3Int :: (Int, Int, Int) -> Int
min3Int (a, b, c) = if min2Int(a, b) == min2Int(a, c)
    then a
    else if min2Int(b, a) == min2Int(b, c)
        then b
    else c

toUpper :: Char -> Char
toUpper c = toEnum (fromEnum c - 32)

toLower :: Char -> Char
toLower c = toEnum (fromEnum c + 32)

isDigit :: Char -> Bool
isDigit c = (fromEnum c >= 48 && fromEnum c <= 57)


charToNum :: Char -> Int
charToNum c = fromEnum c

romanDigit :: Char -> String
romanDigit '1' = "I"
romanDigit '2' = "II"
romanDigit '3' = "III"
romanDigit '4' = "IV"
romanDigit '5' = "V"
romanDigit '6' = "VI"
romanDigit '7' = "VII"
romanDigit '8' = "VIII"
romanDigit '9' = "IX"
romanDigit _   = error "Input must be a digit between 1 and 9"
