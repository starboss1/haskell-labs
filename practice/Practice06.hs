{-# OPTIONS_GHC -Wall #-}
module Practice06 where

import Data.List
type Graph = [[Int]]
g1::Graph
g1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
g2::Graph
g2 = [[1,2,3,4],[0,2,3],[0,1,3,4],[0,1,2],[0,2]]


adj :: Graph -> Int -> [Int]
adj g v = g !! v

nodes :: Graph -> [Int]
nodes g = [0..(length g)-1]

edgeIn :: Graph -> (Int,Int) ->Bool
edgeIn g (x,y) = elem y (g !! x)

edges :: Graph -> [(Int, Int)]
edges g = [(x,y)| x <- nodes g, y <- (adj g x)]

subsets :: [Int] -> [[Int]]
subsets [] = [[]]
subsets (x:xs) = let xss = subsets xs
                 in (map (x:) (xss)) ++ xss

allEdges :: [Int] -> [(Int,Int)]
allEdges xs = [(x,y) | x<-xs, y <- xs, x/=y]


isClique :: Graph -> [Int] -> Bool
isClique gr xs = let es = allEdges xs
                 in null $ filter (not . (edgeIn gr)) es 
cliqueNum :: Graph -> Int
cliqueNum gr = let xss = subsets (nodes gr)
                   qs = filter (isClique gr) xss
               in maximum $ map length qs

goNodes :: Graph -> Int -> [Int]
goNodes gr v = snd $ until cond (oneStep gr) ([v],[]) 

cond :: ([Int],[Int]) -> Bool
cond (new, _) = null new
oneStep :: Graph -> ([Int],[Int]) -> ([Int],[Int])
oneStep gr (ns, os) =
    let old = ns ++ os
        ns1 = concatMap (\x -> adj gr x) ns
        ns2 = filter (`notElem` old) ns1
        new = nub ns2
    in (new,old)