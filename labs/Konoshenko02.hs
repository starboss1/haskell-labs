{-# OPTIONS_GHC -Wall #-}
module Konoshenko02 where

-- Задача 1 -----------------------------------------
sumFl :: [Integer] -> Integer
sumFl [] = 0
sumFl xs  = foldl (+) 0 xs 
  
-- Задача 2 -----------------------------------------
productFr :: [Integer] -> Integer
productFr [] = 0
productFr xs = foldr (*) 1 xs

-- Задача 3 -----------------------------------------
concatFr :: [Int] -> [Int] -> [Int]
concatFr xs ys  = foldr (\x y -> x:y) ys xs


-- Задача 4 -----------------------------------------
insert :: [Int] -> Int -> [Int]
insert xs v = [e | e <- xs, e<v] ++ [v] ++ [e | e <- xs, e > v]

sortInsert :: [Int] -> [Int]
sortInsert  xs = foldl insert [] xs

-- Задача 5 -----------------------------------------
findIndices ::(Int -> Bool) -> [Int] -> [Int] 
findIndices p xs = map fst ( filter (p . snd) ( zip [0..] xs))

-- Задача 6 -----------------------------------------
allReverse :: [String] -> [String]
allReverse xss = reverse (map reverse xss)

-- Задача 7 -----------------------------------------
isDigit :: Char -> Bool
isDigit x = elem x ['0'..'9']

noDigits :: String -> String
noDigits xs = filter (\e -> not (isDigit e)) xs

-- Задача 8 -----------------------------------------
cntGood :: [Int -> Bool] -> Int -> Int
cntGood [] _ = 0
cntGood (p:ps) v | p v = 1 + cntGood ps v 
                 | otherwise = cntGood ps v

-- Задача 9 -----------------------------------------
row :: [Integer] -> [Integer]
row xs = zipWith (+) ([0] ++ xs) (xs ++ [0])

trianglePas :: [[Integer]]
trianglePas = iterate row [1]

-- Задача 10 -----------------------------------------
factorialsM :: [Integer]
factorialsM = zipWith (*) [1..] (1:factorialsM)

