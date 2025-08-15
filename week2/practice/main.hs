main = do
    print vari
    print ( isAutistic vari )
    print ( variv2 )
    print ( patternV2 pepe )
    print ( caseHandle "Hello" )

-- enums
data Thing = A
    | B Bool --enums can have types!
    | C Int
    | D Int String Double Int Bool Double String
    | E Int Int
    deriving Show -- make types printable

vari :: Thing
vari = B (0==0)

isAutistic :: Thing -> Bool
isAutistic A = True
isAutistic _ = False -- _ == default

-- results like in Rust
data ResultInt = Ok Int
    | Err

-- enums can hold multiple types
variv2 :: Thing
variv2 = D 13 "trece" 13.0 13 True 13.0 "que se me crece"

-- algebraic pattern matching
patternV1 :: Thing -> Bool
patternV1 (B a) = True
patternV1 (E a b) = True

-- more pattern matching
data Person = Person String Int Thing
    deriving Show
pepe :: Person
pepe = Person "Pepe" 69 A

patternV2 :: Person -> String

-- match nested types
patternV2 p@(Person n _ A) =
    "Pepe is very autistic"

-- use child AND parent variables
patternV2 p@(Person n _ _) =
    "Person with values <<" ++ show p ++ ">> is " ++ n

-- case expressions
caseHandle :: String -> String
caseHandle s = case s of
    [] -> "What"
    ('H':s) -> "hello too!"
    _ -> "watt"

-- recursive datatypes
data Tree = Leaf Char
    | Node Tree Int Tree

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))
