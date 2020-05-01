{- ##################################
 Rishi Parida
 Homework 5.
 ################################## -}

module Prog5 where

reverse' :: [a] -> [a]
reverse' fst = case fst of
  [] -> []
  (x:xs) -> reverse' xs ++ [x]

isPalindrome :: String -> Bool
isPalindrome s = case s of
  [] -> True
  [c] -> True
  s -> head s == head (reverse s) && isPalindrome(tail(init s))

type TimeStamp = (Int, Int, Int)

isLonger :: TimeStamp -> TimeStamp -> Int
isLonger (x,y,z) (a,b,c)
  | x > a = -1
  | x < a = 1
  | y > b = -1
  | y < b = 1
  | z > c = -1
  | z < c = 1
  | otherwise = 0

totalSeconds :: TimeStamp -> Int
totalSeconds (h, m, s) = (h*3600) + (m*60) + s

isValid :: TimeStamp -> Bool
isValid (h, m, s) = (h >= 0) && (m >= 0) && (m < 60) && (s >= 0) && (s < 60)

time2Str :: TimeStamp -> String
time2Str (h, m, s) = addTZero (show h) ++ ":" ++ addTZero (show m)  ++ ":" ++ addTZero (show s)

addTZero :: String -> String
addTZero s
  | length s == 1 = "0" ++ s
  | otherwise = s

safeFindBefore :: Int -> [Int] -> Maybe [Int]
safeFindBefore _ [] = Just[]
safeFindBefore x xs
  | length ( [a| a <- xs, a == x] ) == 0 = Nothing
  | otherwise = Just (finder x xs)

finder :: Int -> [Int] -> [Int]
finder x xs
  | length(xs) == 0 = []
  | x == head(xs) = []
  | otherwise = head(xs):finder x (tail xs)

data Set = Set [Int]
 | EmptySet
 deriving Show

--Testing set
set1 :: Set
set1 = Set [1,2,3]

member :: Int -> Set -> Bool
member x (Set xs) = elem x xs

size :: Set -> Int
size (Set xs) = length xs

ins :: Int -> Set -> Set
ins x (Set xs)
  | elem x xs = Set xs
  | otherwise = Set (x:xs)
