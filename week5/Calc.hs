{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import ExprT
import Parser
import StackVM

run :: IO ()
run = do
    print $ eval $ ExprT.Mul (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3)) (ExprT.Lit 4)
    print $ evalStr "(2+2+2)*4*3"
    print ( (parseExp lit add mul "(2+2+2)*4*3") :: Maybe ExprT )

    print ( testExp :: Maybe ExprT )
    print ( testExp :: Maybe Integer )
    print ( testExp :: Maybe Bool )
    print ( testExp :: Maybe MinMax )
    print ( testExp :: Maybe Mod7 )

    print $ compile example

    where
        example = "(3 * -4) + 5"
        testExp :: Expr a => Maybe a
        testExp = parseExp lit add mul example


-- exercise 1
eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Mul a b) = (eval a) * (eval b)
eval (ExprT.Add a b) = (eval a) + (eval b)

-- exercise 2
evalStr :: String -> Maybe Integer
evalStr s = getResult evalOperation
    where
        evalOperation = parseExp ExprT.Lit ExprT.Add ExprT.Mul s
        getResult :: Maybe ExprT -> Maybe Integer
        getResult Nothing = Nothing
        getResult (Just str) = Just $ eval str

-- exercise 3
reify :: ExprT -> ExprT
reify = id

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

-- exercise 4
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

data MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
    lit = MinMax
    (MinMax a) `add` (MinMax b) = MinMax $ min a b
    (MinMax a) `mul` (MinMax b) = MinMax $ max a b

data Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit = Mod7 . (`mod` 7)
    (Mod7 a) `add` (Mod7 b) = Mod7 $ (a + b) `mod` 7
    (Mod7 a) `mul` (Mod7 b) = Mod7 $ (a * b) `mod` 7

-- exercise 5
compile :: String -> Maybe Program
compile =  intoProgram . parseExp lit add mul

intoProgram :: Maybe ExprT -> Maybe Program
intoProgram Nothing = Nothing
intoProgram (Just expr) = case expr of
    ExprT.Mul a b ->
        intoProgram (Just a) >>= \aProc ->
        intoProgram (Just b) >>= \bProc ->
        return $ aProc ++ bProc ++ [StackVM.Mul]
    ExprT.Add a b ->
        intoProgram (Just a) >>= \aProc ->
        intoProgram (Just b) >>= \bProc ->
        return $ aProc ++ bProc ++ [StackVM.Add]
    ExprT.Lit a -> Just [PushI a]
