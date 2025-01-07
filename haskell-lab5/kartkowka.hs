function :: IO ()
function = do
    s <- getLine
    n <- return 3
    putStr $ show n ++ s

function' :: IO()
function' = getLine >>= \s -> return 3 >>= \n -> putStr $ show n ++ s


data Tree a = Node a (Tree a) (Tree a)
            | Leaf

paths :: Tree a -> [ [a] ]
paths Leaf = pure []
paths (Node a lt rt) = concat $ ([(a:)] <*>) <$> (fmap paths [lt, rt])


fun :: IO()
fun = do
    s <- getLine
    n <- return 3
    putStrLn $ s ++ show n


func = getLine >>= \l1 -> return (l1 ++ l1) >>= \l2 -> print [l1,l2]

func' = do
    l1 <- getLine
    l2 <- return (l1 ++ l1)
    print [l1, l2]


data Either e a = Left e | Right a

instance Functor (Either e) where
    fmap g (Left e) = Left e
    fmap g (Right a) = Right (g a)


data [] a = [] | a : [a]

instance Applicative [] where
    pure x = [x]
    fs <*> xs =
        [f x | f <- fs, x <- xs]
 