{-# OPTIONS_GHC -Wall #-}
module Practice02 where

cntLess :: [Int] -> Int -> Int
cntLess xs x = length $ filter((>)x)xs

foo :: [Int] -> [Int]
foo xs = map(cntLess xs) xs

cumSumPrefix :: [Int] -> [Int]
cumSumPrefix xs = map sumM (allPrefixs xs)

allPrefixs :: [Int] -> [[Int]]
allPrefixs xs = [take x xs | x <- [1..length xs]]

sumM :: [Int] -> Int
sumM xs = sum xs

diff ::[Int] -> [Int] -> [Int]
diff xs ys = filter (\x -> notInList ys x) xs

notInList :: [Int] -> Int -> Bool
notInList xs x = notElem x xs

minFree :: [Int] -> Int
minFree xs = head (diff [0..] xs)