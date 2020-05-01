{- ##################################
 Rishi Parida
 Homework 2.
 ################################## -}

module Prog2 where

threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent x y z
  | x == y || x == z = False
  | y == z = False
  | otherwise = True

fourDifferent :: Integer -> Integer -> Integer -> Integer -> Bool
fourDifferent x y z t
  | threeDifferent x y z == False = False
  | (threeDifferent x y t == False) || (t == z) = False
  | otherwise = True

sum' :: Integer -> Integer
sum' x
  | x == 1 = 1
  | x > 1 = x + sum' (x-1)
  | otherwise = error "Negative numbers not allowed!"

asciisum :: String -> Integer
asciisum s = sum[toInteger (fromEnum x)| x <- s]

integerSqrt :: Integer -> Integer
integerSqrt x = floor (sqrt (fromIntegral x))

orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (x, y, z) = (minOfThree x y z, midOfThree x y z, maxOfThree x y z)

maxOfThree :: Integer -> Integer -> Integer -> Integer
maxOfThree x y z
  | x >= y && x >= z = x
  | y >= x && y >= z = y
  | z >= x && z >= y = z
  | otherwise = x

minOfThree :: Integer -> Integer -> Integer -> Integer
minOfThree x y z
  | x <= y && x <= z = x
  | y <= x && y <= z = y
  | z <= x && z <= y = z
  | otherwise = x

midOfThree :: Integer -> Integer -> Integer -> Integer
midOfThree x y z
  | x <= y && x >= z = x
  | x >= y && x <= z = x
  | y <= x && y >= z = y
  | y >= x && y <= z = y
  | z <= x && z >= y = z
  | z >= x && z <= y = z
  | otherwise = x

swap :: (Char, Char, Char, Char) -> (Char, Char, Char, Char)
swap (a, b, c, d) = (d, b, c, a)

negateTwoDigits :: [Integer] -> [Integer]
negateTwoDigits xs = [x| x <- xs, x < 10]

matches :: Integer -> [Integer] -> [Integer]
matches n xs = [x| x <- xs, x == n]

element :: Integer -> [Integer] -> Bool
element n xs = (matches n xs) /= []
