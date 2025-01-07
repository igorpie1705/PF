newtype MyInt = MkMyInt Int

instance Eq MyInt where
    (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2

instance Ord MyInt where
    (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2


instance Num MyInt where
  (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
  (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
  (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
  negate (MkMyInt i)            = MkMyInt (negate i)
  abs (MkMyInt i)               = MkMyInt (abs i)
  signum (MkMyInt i)            = MkMyInt (signum i)
  fromInteger int               = MkMyInt (fromIntegral int)


instance Show MyInt where
    show (MkMyInt i) = "MkMyInt " ++ show i


instance Eq a => Eq (BinTree a) where
  -- Puste drzewa są równe
  EmptyBT == EmptyBT = True
  -- Jeśli jedno drzewo jest puste, a drugie nie, to są różne
  EmptyBT == _ = False
  -- Jeśli drugie drzewo jest puste, a pierwsze nie, to są różne
  _ == EmptyBT = False
  -- Dwa węzły są równe, jeśli ich wartości są równe i ich poddrzewa są równe
  (NodeBT x left1 right1) == (NodeBT y left2 right2) =
    x == y && left1 == left2 && right1 == right2


