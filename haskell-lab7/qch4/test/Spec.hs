import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity $ f x

instance Monad Identity where
  return = pure
  Identity x >>= fm = fm x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = liftM Identity arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- Parametryczny MyEither
data MyEither a b = MyLeft a | MyRight b
  deriving (Eq, Show)

instance Functor (MyEither a) where
  fmap _ (MyLeft s) = MyLeft s
  fmap f (MyRight x) = MyRight (f x)

instance Applicative (MyEither a) where
  pure = MyRight
  MyLeft s <*> _ = MyLeft s
  _ <*> MyLeft s = MyLeft s
  MyRight f <*> MyRight x = MyRight (f x)

instance Monad (MyEither a) where
  return = pure
  MyLeft s >>= _ = MyLeft s
  MyRight x >>= f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (MyEither a b) where
  arbitrary = frequency [(1, MyLeft <$> arbitrary), (3, MyRight <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (MyEither a b) where
  (=-=) = eq

data List a = Nil | Cons a (List a) deriving (Show, Eq)

-- Functor instance
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Applicative instance
instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = fmap f xs `append` (fs <*> xs)

-- Monad instance
instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons x xs) >>= f = f x `append` (xs >>= f)

-- Helper function for list append
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (xs `append` ys)

-- Arbitrary instance
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil), (3, liftM2 Cons arbitrary arbitrary)]

-- EqProp instance
instance Eq a => EqProp (List a) where
  (=-=) = eq

main :: IO ()
main = do
  -- Testowanie MyEither
  putStrLn "\n*** Testing MyEither ... ***"
  let t1 = undefined :: MyEither String (Int, String, Int)
  quickBatch $ functor t1
  quickBatch $ applicative t1
  quickBatch $ monad t1

  -- Testowanie List
  putStrLn "\n*** Testing List ... ***"
  let t2 = undefined :: List (Int, String, Int)
  quickBatch $ functor t2
  quickBatch $ applicative t2
  quickBatch $ monad t2

  -- Testowanie Identity
  putStrLn "\n*** Testing Identity ... ***"
  let t3 = undefined :: Identity (Int, String, Int)
  quickBatch $ functor t3
  quickBatch $ applicative t3
  quickBatch $ monad t3
