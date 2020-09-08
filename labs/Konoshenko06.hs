{-# OPTIONS_GHC -Wall #-}
module Konoshenko06 where

import Data.List

type GraphS = (Int,[(Int,Int)])            
type Graph  = [[Int]]

-- Задача 1 -----------------------------------------

isOrdinary :: Graph -> Bool 
isOrdinary gr = and [ (checkOrdVertex gr x)| x <- (nodes gr)]


checkOrdVertex :: Graph -> Int -> Bool
checkOrdVertex gr v  | (nub connectedVert) /= connectedVert = False
                     | elem v connectedVert = False
                     | otherwise = and (map (\e -> elem v (adj gr e)) connectedVert)
                       where connectedVert = adj gr v
                    

-- Задача 2 -----------------------------------------
fromGraph :: Graph -> GraphS
fromGraph [] = (0,[])
fromGraph gr = ((length gr - 1), (edges gr))

-- Задача 3 -----------------------------------------
toGraph :: GraphS -> Graph

toGraph (l,grSL) = [ ((map snd).(filter((==x) . fst)))grSL | x <- [0..l]]

-- Задача 4 -----------------------------------------
shortWay :: Graph -> Int -> Int -> [Int]
shortWay [] _ _ = []
shortWay gr a b | null allShortWays = []
                | otherwise = head allShortWays
                  where allShortWays = shortWays gr a b


-- Задача 5 -----------------------------------------
isConnecting :: Graph -> Bool 
isConnecting gr = (isOrdinary gr) && ((length gr) == length(goNodes gr 0 ))

-- Задача 6 -----------------------------------------
components :: Graph -> [[Int]] 
components gr = nub $ map (goNodes gr )(nodes gr)

-- Задача 7 -----------------------------------------
eccentricity :: Graph -> Int -> Int 
eccentricity gr v = let allRevW = allWays gr v
                        numeredAllRevW = zip allRevW [length allRevW-1,length allRevW-2..0]
                        haveAllVertWays = filter ((containsAllVert gr).fst) numeredAllRevW
                        numOfHaveAllVertWays = map snd haveAllVertWays
                     in if null numOfHaveAllVertWays then -1 else  minimum numOfHaveAllVertWays

containsAllVert :: Graph -> [[Int]] -> Bool
containsAllVert gr xss = ((length . nub.concat)xss) == (length gr)
-- Задача 8 -----------------------------------------
allEccentricity :: Graph -> [Int]
allEccentricity gr = map (eccentricity gr) (nodes gr)

allEccentricityTuple :: Graph -> [(Int,Int)]
allEccentricityTuple gr = map (\e -> (fst e,eccentricity gr(snd e))) 
                                     (zip [0..length (nodes gr)-1] (nodes gr))

findDiameter :: Graph -> Int 
findDiameter gr = maximum (allEccentricity gr)

findRadius :: Graph -> Int 
findRadius gr = minimum (allEccentricity gr)

-- Задача 9 -----------------------------------------
findCenter :: Graph -> [Int] 
findCenter gr = let allEccen = allEccentricityTuple gr
                    radius = findRadius gr
                 in map fst $filter (\e -> (snd e)==radius) allEccen

-- Задача 10 -----------------------------------------
shortWays :: Graph -> Int -> Int -> [[Int]] 
shortWays gr a b = let allRevW = concat $ allWays gr a
                       revWaysEndB = waysEndB allRevW b
                       shortL = shortLength revWaysEndB
                       revShortWays = filter ((== shortL).length) revWaysEndB
                    in sort (reverseLofL revShortWays)
                  

waysEndB :: [[Int]] -> Int -> [[Int]]
waysEndB allRevW b = filter (\(ee:_) -> (ee == b)) allRevW

shortLength :: [[Int]] -> Int
shortLength xs = foldl1 (min) (map (length)xs)

longLength :: [[Int]] -> Int
longLength xs = foldl1 (max) (map (length)xs)

gr3 :: Graph
gr3 = [[1,3],[0,2,3],[1],[0,1]]

allWays :: Graph -> Int -> [[[Int]]]
allWays gr v = until condW (stepW gr) [[[v]]]


condW :: [[[Int]]] -> Bool
condW vsss = null ((head) vsss)

stepW :: Graph -> [[[Int]]] -> [[[Int]]]
stepW gr predR@(wayss:_) = let noCycle = filter ((not . isCycle)) wayss
                            in  [concatMap (\e -> (findWays gr e))noCycle] ++ predR 
stepW _ [] = []
findWays :: Graph -> [Int] -> [[Int]]
findWays _ [] = [[]]
findWays gr lis@(v:_) = let neig = adj gr v
                       in  [[x] ++ lis | x <- neig]

                         

isCycle :: [Int] -> Bool
isCycle vs | length vs == 1 = False
           | otherwise = (length vs) /= ((length . nub) vs)

adj :: Graph -> Int -> [Int]
adj g v = if v < length g then g !! v else []

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

goNodes :: Graph -> Int -> [Int]
goNodes gr v = sort $ snd $ until cond (oneStep gr) ([v],[]) 

cond :: ([Int],[Int]) -> Bool
cond (new, _) = null new
oneStep :: Graph -> ([Int],[Int]) -> ([Int],[Int])
oneStep gr (ns, os) =
    let old = ns ++ os
        ns1 = concatMap (\x -> adj gr x) ns
        ns2 = filter (`notElem` old) ns1
        new = nub ns2
    in (new,old)

reverseLofL :: [[Int]] -> [[Int]]
reverseLofL [] = []
reverseLofL (x : xs) = (reverseLofL xs) ++ [reverse x]

---------------------Òåñòîâ³ äàí³ - Ãðàôè -------
gr1S, gr2S:: GraphS
gr1S = (5,[(0,1),(0,2),(0,3),(1,0),(1,3),(1,4),
           (2,0),(2,4),(2,5),(3,0),(3,1),(4,1),(4,2),(5,2)])
gr2S = (7,[(0,1),(0,3),(1,0),(1,2),(2,1),(2,3),(3,0),(3,2),
           (4,5),(4,6),(5,4),(5,6), (6,4),(6,5)])

gr1, gr2:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]
