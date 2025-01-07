data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)
                 deriving Show

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a) |
              Sub (Expr a) (Expr a) |
              Mul (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (Sub e1 e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"
show' (Mul e1 e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"

dephOfBT :: BinTree a -> Int
dephOfBT EmptyBT = 0
dephOfBT (NodeBT _ lt rt) = 1 + max (dephOfBT lt) (dephOfBT rt)


flattenBTPreorder :: BinTree a -> [a]
flattenBTPreorder EmptyBT = []
flattenBTPreorder (NodeBT x lt rt) = [x] ++ flattenBTPreorder lt ++ flattenBTPreorder rt

flattenBTInorder :: BinTree a -> [a]
flattenBTInorder EmptyBT = []
flattenBTInorder (NodeBT x lt rt) = flattenBTInorder lt ++ [x] ++ flattenBTInorder rt

flattenBTPostorder :: BinTree a -> [a]
flattenBTPostorder EmptyBT = []
flattenBTPostorder (NodeBT x lt rt) = flattenBTPostorder lt ++ flattenBTPostorder rt ++ [x]

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT f EmptyBT = EmptyBT
mapBT f (NodeBT x lt rt) = NodeBT (f x) (mapBT f lt) (mapBT f rt)

insert :: Ord a => a -> BinTree a -> BinTree a
insert x EmptyBT = NodeBT x EmptyBT EmptyBT  -- Jeśli drzewo jest puste, tworzymy nowy węzeł
insert x (NodeBT y lt rt)
  | x < y     = NodeBT y (insert x lt) rt  -- Jeśli x < y, wstawiamy do lewego poddrzewa
  | x > y     = NodeBT y lt (insert x rt)  -- Jeśli x > y, wstawiamy do prawego poddrzewa
  | otherwise = NodeBT y lt rt  -- Jeśli x == y, nie zmieniamy drzewa (brak duplikatów)

list2BST :: Ord a => [a] -> BinTree a
list2BST = foldr insert EmptyBT
