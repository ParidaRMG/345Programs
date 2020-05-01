
{- ##################################
 Rishi Parida
 Homework 7.
 ################################## -}

module Prog7 where

data Expr = Val Int
 | Add Expr Expr
 | Sub Expr Expr
 | Mul Expr Expr
 | Div Expr Expr

--Testing sets
e1, e2, e3 :: Expr
e1 = Val 5
e2 = Add (Val 3) (Val 2)
e3 = Add (Val 3) (Mul (Val 2) (Val 4))
e4 = Mul (Val 2) (Val 4)
e5 = Div (Val 5) (Mul (Val 67) (Val 0))

eval :: Expr -> Int
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y

maxlit :: Expr -> Int
maxlit (Val x) = x
maxlit (Add x y) = maximum( maxlit x : maxlit y : [] )
maxlit (Sub x y) = maximum( maxlit x : maxlit y : [] )
maxlit (Mul x y) = maximum( maxlit x : maxlit y : [] )
maxlit (Div x y) = maximum( maxlit x : maxlit y : [] )

safeeval :: Expr -> Maybe Int
safeeval (Val x) = Just x
safeeval (Div x (Val y))
  | y == 0 = Nothing
  | otherwise = Just (eval x `div` y)
safeeval (Div x y) = safeeval (Div (Val (eval x))  (Val (eval y)))
safeeval e = Just (eval e)

-- show :: Expr -> String
instance Show Expr where
  show (Val x) = show x
  show (Add x y) = "("++ show x ++ "+" ++ show y ++ ")"
  show (Sub x y) = "("++ show x ++ "-" ++ show y ++ ")"
  show (Mul x y) = "("++ show x ++ "*" ++ show y ++ ")"
  show (Div x y) = "("++ show x ++ "/" ++ show y ++ ")"

addone :: Expr -> Expr
addone (Val x) = (Val (x+1))
addone (Add x y) = Add(addone x) (addone y)
addone (Sub x y) = Sub(addone x) (addone y)

containing :: Eq a => [a] -> [a] -> Bool
containing (x:xs) l2
  | length xs == 0 = elem x l2
  | otherwise = elem x l2 && containing xs l2

removeDupicates :: Eq a => [a] -> [a]
removeDupicates [] = []
removeDupicates (x:xs)
  | elem x xs = removeDupicates xs
  | otherwise = x : removeDupicates xs

sumSqNeg :: [Int] -> Int
sumSqNeg xs = sum (map square (filter isNegative xs))

square :: Int -> Int
square x = x*x

isNegative :: Int -> Bool
isNegative x = x < 0

lengths :: [String] -> [Int]
lengths xs = map length xs

total :: (Int -> Int) -> [Int] -> Int
total f x = sum (map f x)

containing' :: Eq a => [a] -> [a] -> Bool
containing' l1 l2 = and(map contains(pairLists l1 l2))

pairLists :: Eq a => [a] -> [a] -> [(a,[a])]
pairLists l1 l2 = [(x, l2)| x <- l1]

contains :: Eq a=> (a,[a]) -> Bool
contains (x, xs) = elem x xs
