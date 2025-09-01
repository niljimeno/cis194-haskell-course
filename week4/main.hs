main :: IO ()
main = do
    let arg1 = [3, 4, 5, 6, 7, 8, 9]
    print $ fun1 arg1
    print $ fun1' arg1

    print $ map fun2 arg1
    print $ map fun2' arg1

    let arg2 = "ABCDEFGIJ"
    print $ foldTree arg2

    let arg3 = [True, False, True, False]
    print $ xor arg3

    let arg4 = [1, 2, 3::Integer]
    print $ map' (+3) arg4
    print $ myFoldl (+) 0 arg4

    print $ sieveSundaram 100


-- exercise 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum
    . filter even
    . takeWhile (>1)
    . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)


-- exercise 2

data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree (x:xs) = Node (depht $ toInteger $ length xs)
    (foldTree $ take half xs)
    x
    (foldTree $ drop half xs)
    where
        len = length xs
        half = quot len 2
        depht = getDepth

getDepth :: Integer -> Integer
getDepth 0 = 0
getDepth x = if mod x 2 > 0 then c+1 else c
    where c = floor ( logBase 2 (fromIntegral x)::Double )


-- exercise 3

xor :: [Bool] -> Bool
xor = (==1) . foldl (+) 0 . map fromEnum

map' :: (a -> a) -> [a] -> [a]
map' f = foldr (\x xs -> (f x) : xs) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f = foldr (\x -> (`f` x))


-- exercise 4

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) remainingNumbers
    where
        cap = 2*n + 2
        remainingNumbers = filter (not . (`elem` toRemove)) [1..cap]
        toRemove = [ i + j + 2*i*j | j <- [1..n], i <- [1..j]]

