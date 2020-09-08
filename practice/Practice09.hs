{-# OPTIONS_GHC -Wall #-}
module Practice09 where

data Branch = Leaf Int | Fork Branch Branch deriving (Eq, Show)

-- br :: Branch
-- br = Fork (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3)) (Leaf 4))

frigle :: Branch -> [Int]
frigle (Leaf v) = [v]
frigle (Fork l r) = frigle l ++ frigle r

height :: Branch -> Int
height (Leaf _) = 0
height (Fork l r) = 1 + max (height l) (height r) 


showB :: Branch -> String
showB (Leaf v) = show v
showB (Fork l r) = "(" ++ showB l ++ " " ++ showB r ++ ")"

minOne :: [Int] -> Branch
minOne [v] = Leaf v
minOne xs = let d = div (length xs) 2
                l = minOne (take d xs)
                r = minOne (drop d xs)
             in Fork l r

brs :: [Int] -> [Branch]
brs [v] = [Leaf v]
brs xs = [Fork l r | i <- [1..length xs-1], l <- brs(take i xs), r <- brs(drop i xs)]

takeMinBr :: [Branch] -> [Branch]
takeMinBr bs = let h = minimum (map (height)bs)
                in filter ((==h) . height)bs

minBr :: [Int] -> [Branch]
minBr xs = takeMinBr (brs xs)