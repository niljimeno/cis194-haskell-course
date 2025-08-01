main = do
    dni
    towerOfHanoi

-- dni exercise
dni = do
    putStrLn "starting exercise 1"
    print ( verifyDNI 4012888888881881 )
    print ( verifyDNI 4012888888881882 )

toDigitsRev :: Int -> [Int]
toDigitsRev n
    | n <= 0 = []
    | otherwise = ( mod n 10 ) : toDigits (div n 10)

toDigits :: Int -> [Int]
toDigits n
    | n <= 0 = []
    | otherwise = toDigits (div n 10) ++ [ mod n 10 ]

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther (x:xs)
    | xs == [] = [x]
    | length xs `mod` 2 == 0 = x : doubleEveryOther xs
    | otherwise = x * 2 : doubleEveryOther xs

sumDigits :: [Int] -> Int
sumDigits (x:xs)
    | xs == [] = x
    | otherwise = (div x 10) + (mod x 10) + sumDigits xs

verifyDNI :: Int -> Bool
verifyDNI n = do
    sumDigits ( doubleEveryOther ( toDigits n )) `mod` 10 == 0


-- hanoi exercise
towerOfHanoi = do
    putStrLn "starting exercise 2"
    print ( hanoi 2 "a" "b" "c" )

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 main aux tar = [(main, tar)]
hanoi n main aux tar =
    hanoi (n-1) main tar aux ++
    [(main, tar)] ++
    hanoi (n-1) aux tar main
