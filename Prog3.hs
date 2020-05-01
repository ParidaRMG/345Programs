{- ##################################
 Rishi Parida
 Homework 3.
 ################################## -}

module Prog3 where
import Data.Char


doubleAll :: [Int] -> [(Int, Int)]
doubleAll xs = [(x, 2*x)| x <- xs]

productLastPart :: Int -> [Int] -> Int
productLastPart n xs = product (drop (length xs - n) xs)

init' :: [Int] -> [Int]
init' xs = take (length xs - 1) xs

nestedParens :: String -> Bool
nestedParens s
  | length s == 1 = False
  | s == "()" = True
  | s!!0 == '(' = nestedParens (tail (init s))
  | otherwise = False

triads :: Int -> [(Int,Int,Int)]
triads n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], ((x*x) + (y*y)) == (z*z)]

pushRight :: String -> Int -> String
pushRight s n = [' '| x <- [1..(n - length s)]] ++ s

lowerFirstCharacter :: String -> String
lowerFirstCharacter s = [toLower (s!!0)] ++ tail s

middleWord :: String -> String
middleWord s = take (((findSpaces s 0)!!1 - (findSpaces s 0)!!0) - 1) ((drop (((findSpaces s 0)!!0) + 1) s))

findSpaces ::  String -> Int -> [Int]
findSpaces s x
  | length s == 0 = []
  | head s == ' ' = [x] ++ findSpaces (tail(s)) (x+1)
  | otherwise = findSpaces (tail(s)) (x+1)

findWord :: String -> [String]
findWord s = [s]

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
