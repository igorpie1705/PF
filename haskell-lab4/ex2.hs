type X = Int
type Y = Int

data CartInt2DVec = MkCartInt2DVec X Y

xCoord :: CartInt2DVec -> X
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Y
yCoord (MkCartInt2DVec _ y) = y

data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y


data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}


data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL = error "head' : the empty list has no head!"
head' (Cons x xs) = x

data ThreeColors = Blue |
                  White |
                  Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red = "Irene Jacob"


data Cart3DVec a = Cart3DVec a a a

xCoord''' :: Cart3DVec a -> a
yCoord''' :: Cart3DVec a -> a
zCoord''' :: Cart3DVec a -> a

xCoord''' (Cart3DVec x _ _) = x
yCoord''' (Cart3DVec _ y _) = y
zCoord''' (Cart3DVec _ _ z) = z

data Cart3DVec' a = Cart3DVec' { xCoord'''' :: a, yCoord'''' :: a, zCoord'''' :: a }



data Shape = Circle Float |
             Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rectangle a b) = a * b

data Tree a = EmptyT 
            | Node a (Tree a) (Tree a)
            deriving Show

rootValue :: Tree a -> a
rootValue EmptyT = error "Empty tree has no root value"
rootValue (Node value _ _) = value

type Action = String

data TrafficLights = Redd
                     | Yelloww
                     | Greenn
    deriving (Show, Eq)

actionFor :: TrafficLights -> Action
actionFor Redd = "Stop"
actionFor Yelloww = "Prepare to stop"
actionFor Greenn = "Go"