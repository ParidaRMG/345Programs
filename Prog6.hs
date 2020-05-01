{- ##################################
 Rishi Parida
 Homework 6.
 ################################## -}
module Prog6 where

data Set = Set [Int]
 | EmptySet
 deriving Show

--Testing sets
set1 :: Set
set1 = Set [1,2,3]
set2 :: Set
set2 = Set[1,2,3,4]

equal :: Set -> Set -> Bool
equal (Set xs) (Set ys) = xs == ys

saferemove :: Int -> Set -> Maybe Set
saferemove x (Set xs)
  | not (elem x xs) = Nothing
  | otherwise = Just  (Set([a| a <- xs, a /= x]))

union :: Set -> Set -> Set
union (Set xs) (Set ys) = Set(xs ++ ys)

data Tree = Leaf Int
 | Node Tree Int Tree
 deriving Show

--Testing sets
t1 :: Tree
t1 = (Node (Leaf 2) 3 (Leaf 4))
t2 = (Node (Leaf 2) 3 (Node (Leaf 5) 4 (Node (Node (Leaf 9) 10 (Leaf 11)) 6 (Leaf 8))))
t3 = (Node (Leaf 2) 3 (Node (Leaf 5) 4 (Node (Leaf 7) 6 (Leaf 8))))
t4 = (Node (Node (Node (Node (Leaf 10) 11 (Leaf 12)) 13 (Leaf 14)) 15 (Leaf 16)) 17 (Node (Leaf 19) 18 (Leaf 20)))
t5 = (Node (Node (Node (Node (Leaf 1) 1 (Leaf 1)) 0 (Leaf 1)) 0 (Leaf 0)) 1 (Node (Leaf 1) 1 (Leaf 0)))

preorder :: Tree -> [Int]
preorder (Leaf x) = [x]
preorder (Node a y b) = [y] ++ preorder (a) ++ preorder (b)

postorder :: Tree -> [Int]
postorder (Leaf x) = [x]
postorder (Node a y b) = preorder (a) ++ [y] ++ preorder (b)

countZeros :: Tree -> Int
countZeros (Leaf x)
  | x == 0 = 1
  | otherwise = 0
countZeros (Node a y b)
  | y == 0 = countZeros (a) + 1 + countZeros(b)
  | otherwise = countZeros(a) + countZeros(b)

countLeaves :: Tree -> Int
countLeaves (Leaf x) = 1
countLeaves (Node a y b) = countLeaves (a) + 1 + countLeaves (b)

countInteriorNodes :: Tree -> Int
countInteriorNodes (Leaf x) = 0
countInteriorNodes (Node a y b) = countInteriorNodes(a) + 1 + countInteriorNodes(b)

depth :: Tree -> Int
depth (Leaf x) = 1
depth tr = maximum (depthHelper tr 0)

depthHelper :: Tree -> Int -> [Int]
depthHelper (Leaf x) l = [l]
depthHelper (Node a y b) l = depthHelper a (l+1) ++ depthHelper b (l+1)

balanced :: Tree -> Bool
balanced (Leaf x) = True
balanced (Node a y b) = balanced a && checkBalance a b && balanced b

checkBalance :: Tree -> Tree -> Bool
checkBalance (Leaf x) (Leaf y) = checkDiff x y
checkBalance (Node _ x _) (Node _ y _) = checkDiff x y
checkBalance (Node _ x _) (Leaf y) = checkDiff x y
checkBalance (Leaf x) (Node _ y _) = checkDiff x y

checkDiff :: Int -> Int -> Bool
checkDiff x y = (x - y) == (-1) || (x-y) == 1 || (x-y) == 0
