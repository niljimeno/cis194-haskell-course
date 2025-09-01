-- i'm not in uni so no comments here.
module Golf where

run :: IO ()
run = do
    print $ skips "hello"
    print $ skips [3::Int, 2, 1]
    print $ localMaxima [3, 2, 1, 4, 2]
    putStrLn $ histogram [8, 2, 1, 4, 2, 4, 9, 8, 4]


-- exercise 1

skips :: [a] -> [[a]]
skips [] = [[]]
skips [x] = [[x]]
skips input@(_:xs) = input : (skips xs)


-- exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima (l:rest@(current:r:_))
    | current > l && current > r = current : next
    | otherwise = next
    where next = localMaxima rest
localMaxima _ = []


-- exercise 3

histogram :: [Integer] -> String
histogram n = concat $ map println (reverse [1..maximum counts]) ++ [rest]
    where
        counts = map (\i -> count i n) [1..9]
        println i = [ getStar i (counts!!(col-1)) | col <- [1..9]] ++ "\n"
        rest = "=========\n123456789"

count :: Integer -> [Integer] -> Integer
count _ [] = 0
count x (n:ns)
    | x == n = 1 + count x ns
    | otherwise = count x ns

getStar :: Integer -> Integer -> Char
getStar h c
    | c < h = ' '
    | otherwise = '*'
