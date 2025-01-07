data Foo a = MkFoo { value :: a, name :: String }
instance Show a => Show (Foo a) where
    show(MkFoo {value = v, name = n}) = "Foo " ++ show n ++ " with " ++ show v


data T1 a b = B (a,b) | A
instance (Eq a, Eq b) => Eq (T1 a b) where
  (==) (B (a1, b1)) (B (a2, b2)) = a1 == a2 && b1 == b2
  (==) A A = True
  (==) _ _ = False


data BST a = EmptyBST | NodeBST a (BST a) (BST a)
isElemOfBST :: Ord a => a -> BST a -> Bool
isElemOfBST _ EmptyBST = False
isElemOfBST x (NodeBST e lt lr)
 | x == e = True
 | x < e = isElemOfBST x lt
 | otherwise = isElemOfBST x lr


data BinTree a = NodeBT (BinTree a) (BinTree a) | Leaf a deriving Show