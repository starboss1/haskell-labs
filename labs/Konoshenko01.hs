{-# OPTIONS_GHC -Wall #-}
module Konoshenko01 where

-- Задача 1 -----------------------------------------
power3 :: [Integer]
power3 = [x^(3::Integer) | x <- [(1::Integer)..]]

-- Задача 2 -----------------------------------------
toPower3 :: [Integer]
toPower3 = [(3::Integer)^x | x <- [(1::Integer)..]]

-- Задача 3 -----------------------------------------
sumPower3 :: Integer -> Integer
sumPower3 n = sum [(3::Integer)^x::Integer | x <- [1..n]]

-- Задача 4 -----------------------------------------
sumPower :: Integer -> Integer -> Integer
sumPower m n = sum [m^x::Integer | x <- [1..n]]

-- Задача 5 -----------------------------------------
lessMe :: [Int] -> [Int]
lessMe [] = []
lessMe xs = map (\e -> (length (filter (<e) xs))) xs
 
-- Задача 6 -----------------------------------------
frequency :: [Int] -> [(Int,Int)]
frequency [] = []
frequency (x:xs) = (x, length ( filter (== x) ( x:xs))) : frequency (filter (/= x) ( xs))

-- Задача 7 -----------------------------------------
hailstone :: Int -> Int
hailstone n | n == 1 = 1 | even n = div n 2 | otherwise = 3 * n + 1 

-- Задача 8 -----------------------------------------
hailSeq :: Int -> [Int]
hailSeq n | n == 1 = [1] |otherwise = n : hailSeq (hailstone n)

-- Задача 9 -----------------------------------------
allHailSeq :: [[Int]]
allHailSeq = [xs | xs <- map (hailSeq) [1..]]

-- Задача 10 -----------------------------------------
firstHailSeq :: Int -> Int
firstHailSeq l = (head (head (filter (\n -> length n == l) allHailSeq)))