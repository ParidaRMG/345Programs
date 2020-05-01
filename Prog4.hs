{- ##################################
 Rishi Parida
 Homework 4.
 ################################## -}

module Prog4 where

import Data.Char


doubleAll :: [Int] -> [(Int, Int)]
doubleAll [] = []
doubleAll x = [(head x, head (x) *2)] ++ doubleAll (tail x)

productLastPart :: Int -> [Int] -> Int
productLastPart x y
  | length y > x = productLastPart x (tail y)
  | length y == x = sumList y

--Gets the sum of a list (since list comprehension is banned)
sumList :: [Int] -> Int
sumList [] = 1
sumList x = (head x) * sumList (tail x)

init' :: [Int] -> [Int]
init' xs = reverse (tail (reverse xs))

lowerOddLetters :: String -> String
lowerOddLetters s
  | length s < 2 = s
  | otherwise = toLower (head s) : head (tail s) : lowerOddLetters (tail (tail s))

replicate' :: Int -> Char -> String
replicate' 1 c = [c]
replicate' x c = [c] ++ replicate' (x - 1) c

-- COME BACK TO THIS ONE
iSort' :: [(Int, String)] -> [(Int, String)]
iSort' [] = []
iSort' (x:xs) = ins' x (iSort' xs)

ins' :: (Int,String) -> [(Int, String)] -> [(Int, String)]
ins' (x,y) [] = [(x,y)]
ins' x (y:ys)
    |fst x < fst y = x:y:ys 
    |otherwise = y:ins' x ys

lowerFirstCharacter :: String -> String
lowerFirstCharacter s = toLower (head s) : (tail s) 

middleWord :: String -> String
middleWord s = take (((findSpaces s 0)!!1 - (findSpaces s 0)!!0) - 1) ((drop (((findSpaces s 0)!!0) + 1) s))

findSpaces ::  String -> Int -> [Int]
findSpaces s x
  | length s == 0 = []
  | head s == ' ' = [x] ++ findSpaces (tail(s)) (x+1)
  | otherwise = findSpaces (tail(s)) (x+1)

lowerFirstLetter :: String -> String
lowerFirstLetter s = lowerTheFirst s 0

lowerTheFirst :: String -> Int -> String
lowerTheFirst s n
  | n >= length s = s
  | isUpper (s!!n) = ((take (n) s) ++ [toLower (s!!n)]) ++ drop (n + 1) s
  | otherwise = lowerTheFirst s (n+1)

lowerFirstTwoLetters :: String -> String
lowerFirstTwoLetters s = lowerTheRealFirst s 0

lowerTheRealFirst :: String -> Int -> String
lowerTheRealFirst s n
  | n >= length s = s
  | isUpper (s!!n) = lowerTheFirst (((take (n) s) ++ [toLower (s!!n)]) ++ drop (n + 1) s) n
  | otherwise = lowerTheRealFirst s (n+1)
