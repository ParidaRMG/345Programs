{- ##################################
 Rishi Parida
 Homework 1.
 ################################## -}

module Prog1 where

isPositive :: Float -> Bool
isPositive x = x > 0

dividesEvenlyByFive :: Integer -> Bool
dividesEvenlyByFive x = (x `mod` 5) == 0

--COME BACK TO THIS ONE
middle :: Integer -> Integer -> Integer -> Integer
middle x y z
  | x >= y && y >= z = y
  | x >= z && z >= y = z
  | y >= x && x >= z = x
  | y >= z && z >= x = z
  | z >= y && y >= x = y
  | z >= x && x >= y = x
  | otherwise = y --Come back to this one

nor :: Bool -> Bool -> Bool
nor a b = (a == False) && (b == False)

triangleArea :: Integer -> Integer -> Float
triangleArea b h = 1.5 * fromIntegral(b * h)

ceilingDecimal :: Float -> Float
ceilingDecimal x = fromInteger (ceiling x) :: Float

letterGrade :: Integer -> String
letterGrade x
  | x >= 93 = "A"
  | x >= 90 = "A-"
  | x >= 87 = "B+"
  | x >= 83 = "B"
  | x >= 80 = "B-"
  | x >= 77 = "C+"
  | x >= 73 = "C"
  | x >= 70 = "C-"
  | x >= 67 = "D+"
  | x >= 63 = "D"
  | x >= 60 = "D-"
  | x >= 0 = "F"
  | otherwise = "Sorry, not a valid score!"

averageThree :: Integer -> Integer -> Integer -> Float
averageThree x y z = fromIntegral(x + y + z) / 3.0

howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage x y z
  | fromIntegral(x) > averageThree x y z && fromIntegral(y) > averageThree x y z = 2
  | fromIntegral(x) > averageThree x y z && fromIntegral(z) > averageThree x y z = 2
  | fromIntegral(y) > averageThree x y z && fromIntegral(z) > averageThree x y z = 2
  | fromIntegral(x) > averageThree x y z = 1
  | fromIntegral(y) > averageThree x y z = 1
  | fromIntegral(z) > averageThree x y z = 1
  | otherwise = 0

howManyWithinThreshold :: Integer -> Integer -> Integer -> Float -> Integer
howManyWithinThreshold x y z t
  |threshCheck (fromIntegral(x)) (averageThree x y z) t && threshCheck (fromIntegral(y)) (averageThree x y z) t && threshCheck (fromIntegral(z)) (averageThree x y z) t = 3
  |threshCheck (fromIntegral(x)) (averageThree x y z) t && threshCheck (fromIntegral(y)) (averageThree x y z) t = 2
  |threshCheck (fromIntegral(x)) (averageThree x y z) t && threshCheck (fromIntegral(z)) (averageThree x y z) t = 2
  |threshCheck (fromIntegral(y)) (averageThree x y z) t && threshCheck (fromIntegral(z)) (averageThree x y z) t = 2
  |threshCheck (fromIntegral(x)) (averageThree x y z) t = 1
  |threshCheck (fromIntegral(y)) (averageThree x y z) t = 1
  |threshCheck (fromIntegral(z)) (averageThree x y z) t = 1
  | otherwise = 0

threshCheck :: Float -> Float -> Float -> Bool
threshCheck x a t = (x <= a + t) && (x >= a - t)
