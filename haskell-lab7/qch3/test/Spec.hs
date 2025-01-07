import Control.Monad
import Test.QuickCheck

data DaysOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq, Show)

dayAfter :: DaysOfWeek -> DaysOfWeek
dayAfter Mon = Tue
dayAfter Tue = Wed
dayAfter Wed = Thu
dayAfter Thu = Fri
dayAfter Fri = Sat
dayAfter Sat = Sun
dayAfter Sun = Mon

prop_WeekCycle :: DaysOfWeek -> Bool
prop_WeekCycle d = sevenTimesNext d == d
  where sevenTimesNext d' = iterate dayAfter d' !! 7

instance Arbitrary DaysOfWeek where
  arbitrary = elements [ Mon, Tue, Wed, Thu, Fri, Sat, Sun ]

data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency
    [ (1, return Nil)  -- 1 na 5 razy pustą listę
    , (4, liftM2 Cons arbitrary arbitrary)  -- 4 na 5 razy listy o długości 1 lub więcej
    ]

reverse' :: List a -> List a
reverse' = go Nil
  where go acc Nil = acc
        go acc (Cons x xs) = go (Cons x acc) xs

prop_RevRevList_Eq_List :: List Int -> Bool
prop_RevRevList_Eq_List xs = reverse' (reverse' xs) == xs

length' :: List a -> Int
length' Nil = 0
length' (Cons _ xs) = 1 + length' xs

prop_RevRevList_Eq_List2 :: List Int -> Property
prop_RevRevList_Eq_List2 xs = collect (length' xs) $ reverse' (reverse' xs) == xs

data BinTree a = EmptyBT | NodeBT (BinTree a) a (BinTree a) deriving Show

insertIntoBinTree :: a -> BinTree a -> BinTree a
insertIntoBinTree x EmptyBT = NodeBT EmptyBT x EmptyBT
insertIntoBinTree x (NodeBT lt a rt) =
  if depthOfBinTree lt <= depthOfBinTree rt
     then NodeBT (insertIntoBinTree x lt) a rt
     else NodeBT lt a (insertIntoBinTree x rt)

depthOfBinTree :: BinTree a -> Int
depthOfBinTree EmptyBT = 0
depthOfBinTree (NodeBT lt _ rt) = 1 + max (depthOfBinTree lt) (depthOfBinTree rt)

elemCountOfBinTree :: BinTree a -> Int
elemCountOfBinTree EmptyBT = 0
elemCountOfBinTree (NodeBT lt _ rt) = 1 + elemCountOfBinTree lt + elemCountOfBinTree rt

prop_InsToBinTreeIncrItsDepthByAtMost1 :: a -> BinTree a -> Bool
prop_InsToBinTreeIncrItsDepthByAtMost1 x t0 =
  depthOfBinTree (insertIntoBinTree x t0) - depthOfBinTree t0 <= 1

prop_InsertIncreasesElemCountByOne :: a -> BinTree a -> Bool
prop_InsertIncreasesElemCountByOne e t =
  elemCountOfBinTree (insertIntoBinTree e t) - elemCountOfBinTree t == 1

instance Arbitrary a => Arbitrary (BinTree a) where
  arbitrary = sized arbitraryBinTree

arbitraryBinTree :: Arbitrary a => Int -> Gen (BinTree a)
arbitraryBinTree 0 = return EmptyBT
arbitraryBinTree n = frequency
  [ (1, return EmptyBT)
  , (3, liftM3 NodeBT (arbitraryBinTree (n `div` 2))
                     arbitrary
                     (arbitraryBinTree (n `div` 2))) ]

main :: IO ()
main = do
  putStrLn "\n*** Testing prop_WeekCycle... ***"
  quickCheck prop_WeekCycle
  putStrLn "\n*** Testing prop_RevRevList_Eq_List... ***"
  quickCheck prop_RevRevList_Eq_List
  putStrLn "\n*** Testing prop_RevRevList_Eq_List2... ***"
  quickCheck prop_RevRevList_Eq_List2
  putStrLn "\n*** Testing prop_InsToBinTreeIncrItsDepthByAtMost1... ***"
  quickCheck (prop_InsToBinTreeIncrItsDepthByAtMost1 :: Int -> BinTree Int -> Bool)
  putStrLn "\n*** Testing prop_InsertIncreasesElemCountByOne... ***"
  quickCheck (prop_InsertIncreasesElemCountByOne :: Int -> BinTree Int -> Bool)
