rats = putStrLn "Hello, World!"

sumtorial :: Int -> Int
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

hailstone :: Int -> Int
hailstone n
    | mod n 2 == 0 = div n 2
    | otherwise = 3*n + 1

sumPair :: (Int, Int) -> Int
sumPair (n, m) = n + m

normieSum :: Int -> Int -> Int
normieSum n m = n + m

list :: [Char]
list = [ 'h', 'e', 'l', 'l', 'o' ]

construct :: Int -> [ Int ]
construct 0 = [ 0 ]
construct n = n : (construct (n-1))

sumFun :: [ Int ] -> Int
sumFun [] = 0
sumFun (n:ns) = n + sumFun ns

main = do
    print ( sumtorial 209 )
    print ( hailstone 1984 )
    print ( even 2 )
    print ( sumPair (2, 4) )
    print ( normieSum 2 4 )
    print ( construct 20 )
    print ( sumFun [20, 13, 2, 20] )
    print ( "hello" ++ " " ++ "world" )
    rats
