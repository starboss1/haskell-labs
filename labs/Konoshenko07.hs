{-# OPTIONS_GHC -Wall #-}
module Konoshenko07 where

import Data.List(sort)

data BinTreeM a = EmptyM 
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                   deriving (Show, Eq) 
-- B-дерево порядка t (NodeB kl tl) =>  
--      t-1 <= length kl <= 2*t-1  &&  t <= length tl <= 2*t
data Btree a =  NodeB [a] [Btree a]  deriving (Show, Eq)
-- головні характеристики B-дерево  (BInform heigth min max)
data BInform a = BInform {hB::Int, minB::a, maxB::a} deriving (Show, Eq)


-- Задача 1 -----------------------------------------
isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch EmptyM = False
isSearch (NodeM v k tl tr) = k > 0 && checkNode LT v tl && checkNode GT v tr

checkNode :: (Ord a) =>Ordering -> a ->BinTreeM a -> Bool
checkNode order v1 (NodeM v2 k tl tr) = (order == compare v2 v1) && k >0
                                        && checkNode LT v2 tl && checkNode GT v2 tr
checkNode _ _ EmptyM = True

-- Задача 2 -----------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch EmptyM _ = False
elemSearch (NodeM e _ tl tr) v = case compare v e of
                                     LT -> elemSearch tl v
                                     GT -> elemSearch tr v
                                     EQ -> True

-- Задача 3 -----------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
insSearch EmptyM v = NodeM v 1 EmptyM EmptyM
insSearch (NodeM e k tl tr) v = case compare v e of
                                    LT -> NodeM e k(insSearch tl v) tr
                                    GT -> NodeM e k tl (insSearch tr v)
                                    EQ -> NodeM e (k+1) tl tr



-- Задача 4 -----------------------------------------
delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
delSearch EmptyM _ = EmptyM 
delSearch (NodeM e k tl tr) v = case compare v e of
                                    LT -> NodeM e k (delSearch tl v) tr
                                    GT -> NodeM e k tl (delSearch tr v)
                                    EQ -> if k == 1 then replaceNode tl tr
                                          else  (NodeM e (k-1) tl tr)
replaceNode :: (Ord a) => BinTreeM a -> BinTreeM a -> BinTreeM a
replaceNode tl EmptyM = tl
replaceNode EmptyM tr = tr
replaceNode (NodeM e k tl tr) tr2 = NodeM e k tl(replaceNode tr tr2)

-- Задача 5 -----------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList l = toList $ foldl insSearch EmptyM l 

toList :: (Ord a) => BinTreeM a -> [a]
toList EmptyM = []
toList (NodeM e k tl tr) = toList tl ++ [e | _ <- [1..k]] ++ toList tr

-- Задача 6 -----------------------------------------

minKey :: (Bounded a, Ord a) => Btree a -> a
minKey (NodeB ks []) = head ks
minKey (NodeB _ ns) = minKey $ head ns

maxKey :: (Bounded a, Ord a) => Btree a -> a
maxKey (NodeB ks []) = last ks
maxKey (NodeB _ ns) = maxKey $ last ns

heig :: (Bounded a, Ord a) => Btree a -> Int
heig (NodeB _ []) = 0
heig (NodeB _ ns) = 1 + (heig $ head ns)

findBInform :: (Bounded a, Ord a) => Btree a ->  BInform a
findBInform tr = BInform  (heig tr)  (minKey tr) (maxKey tr)

-- Задача 7 -----------------------------------------
isBtree  :: (Bounded a, Ord a) => Int -> Btree a -> Bool 
isBtree t tr@(NodeB kl kt) = nodeIsNodeB t tr && nodesIsNodeB t kt && correctChildKeys kl kt


nodesIsNodeB :: (Bounded a, Ord a) => Int -> [Btree a] -> Bool
nodesIsNodeB _ [] = True
nodesIsNodeB t ((NodeB kl kt):xs) = nodeIsNodeB t (NodeB kl kt) && (nodesIsNodeB t kt) 
                                    && (nodesIsNodeB t xs) && correctChildKeys kl kt


nodeIsNodeB :: (Bounded a, Ord a) => Int -> Btree a -> Bool
nodeIsNodeB t (NodeB kl kt) = (length kl >= 1) && (length kl <= (2*t-1))
                                 && ((length kt == (length kl) + 1) || length kt == 0)
                                 && isNonDecendind kl

isNonDecendind :: (Bounded a, Ord a) => [a] -> Bool
isNonDecendind [] = True
isNonDecendind [_] = True
isNonDecendind (x:y:xs) = (x<=y) && isNonDecendind xs

correctChildKeys :: (Bounded a, Ord a) => [a] -> [Btree a] -> Bool
correctChildKeys (x:xs) ((NodeB kl _):(NodeB kl2 kt2):nods) = 
         (all (<=x) kl && all (>=x)kl2) && correctChildKeys xs ((NodeB kl2 kt2):nods)
correctChildKeys (x:xs) ((NodeB kl _):nods) = all (<=x) kl && correctChildKeys xs nods
correctChildKeys _ _ = True

-- Задача 8 -----------------------------------------
bTreeToList :: (Bounded a, Ord a) => Btree a -> [a]
bTreeToList (NodeB kl []) = kl
bTreeToList (NodeB kl kt) = kl ++ (concatMap (bTreeToList) kt)

eqBtree :: (Bounded a, Ord a) => Int -> Btree a -> Btree a -> Bool 
eqBtree _ tr1 tr2 = sort(bTreeToList tr1) == sort(bTreeToList tr2)

-- Задача 9 -----------------------------------------
elemBtree :: Ord a => Btree a -> a -> Bool
elemBtree (NodeB kl kt) v   |elem v kl = True
                            |null kl || null kt = False
                            |ind == -1 = elemBtree (last kt) v 
                            |otherwise = elemBtree (kt !! ind) v
                             where ind = position v kl


-- Задача 10 -----------------------------------------
insBtree :: Ord a => Int -> Btree a -> a -> Btree a
insBtree t tr@(NodeB _ _) v  = if isFull t tr then insertNoFull t (NodeB [mid] [leftNode,rightNode]) v
                                 else insertNoFull t tr v
                                 where (leftNode, mid, rightNode) = splitAtB t tr

insertNoFull :: Ord a => Int -> Btree a -> a -> Btree a
insertNoFull _ (NodeB kl []) v = NodeB (insertKey v kl) []
insertNoFull t (NodeB kl tl) v = let (kl1, kl2, tl1, bt, tl2) = decomposeNodeB v kl tl
                                     (bt1, mid, bt2) = splitAtB t bt
                                     tr1 = if v <= mid then insertNoFull t bt1 v else bt1
                                     tr2 = if v <= mid then bt2 else insertNoFull t bt2 v
                                  in if isFull t bt then (NodeB (kl1 ++ (mid:kl2)) (tl1 ++ (tr1:(tr2:tl2))))
                                    else NodeB kl (tl1 ++ ((insertNoFull t bt v):tl2))


isFull :: Ord a => Int -> Btree a -> Bool
isFull t (NodeB keys _) = length keys ==2*t-1

insertKey :: Ord a => a -> [a] -> [a]
insertKey e [] = [e]
insertKey e l@(x:xs) | e > x = [x] ++ insertKey e xs
                     | e <= x = [e] ++ l
insertKey  _ (_:_) = []

position :: Ord a => a -> [a] -> Int
position v xs | null res = -1
              | otherwise = head res
                where res = findInd v xs 

findInd :: Ord a => a -> [a] -> [Int]
findInd v xs = [ i | (x,i) <- zip xs [0..], v <= x]



decomposeNodeB :: Ord a => a -> [a] -> [Btree a] -> 
                        ([a], [a], [Btree a], Btree a, [Btree a])
decomposeNodeB u kl tl = (kl1, kl2, tl1, tl !! n, tl2)
                         where n = position u kl
                               tl1 = [tl!! x |x <- [0..n-1]]
                               tl2 = [tl!! x | x <- [n+1..(length tl)-1]]
                               kl1 = [kl!! x | x <- [0..n-1]]
                               kl2 = [kl!! x | x <- [n..(length kl)-1]]

splitAtB :: Ord a => Int -> Btree a -> (Btree a, a, Btree a)
splitAtB t (NodeB kl kt) = (NodeB leftKey leftNode, mid, NodeB rightKey rightNode)
                              where mid = kl !! (t-1)
                                    leftKey = take (t-1) kl
                                    rightKey = drop (t) kl
                                    leftNode = take t kt
                                    rightNode = drop (t) kt
                                    

isEmpty :: Ord a => BinTreeM a -> Bool
isEmpty EmptyM = True
isEmpty _ = False

---------------------Тестові дані - Дерева пошуку -------
bm :: BinTreeM Char
bm = NodeM  't' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'w' 2  EmptyM EmptyM)   

tiB1 :: Btree Char 
tiB1 = NodeB ['G','M','P','X'] 
             [ NodeB ['A','C','D','E'] []
             , NodeB ['J','K'] []
             , NodeB ['N','O'] []
             , NodeB ['R','S','T','U','V'] []
             , NodeB ['Y','Z'] [] ]
{-
tiB1 :: Btree Char 
tiB1 = NodeB ['G','M','P','X'] 
             [ NodeB ['A','C','D','E'] []
             , NodeB ['J','K'] []
             , NodeB ['N','O'] []
             , NodeB ['R','S','T','U','V'] []
             , NodeB ['Y','Z'] [] ]

tBtr1 :: Btree Int
tBtr1 = NodeB [5,10,12] [ts0,ts1,ts2,ts3]
   where ts0 = NodeB [1,3  ] []   --- ,4,5] []  --
         ts1 = NodeB [6,6 ,8,9,10] [] --- ,8,9,10] []  -- ] []   
         ts2 = NodeB [11,11,12,12] []
         ts3 = NodeB [16,16] [] -- ,18,19,20] [] 

tBtr2 :: Btree Int 
tBtr2 = NodeB [15] [ts10,ts11]
  where ts10 = NodeB [11,13] [] 
        ts11 = NodeB [21,22] []  
-}
tBt1 :: Btree Char 
tBt1 = NodeB "L"
       [ NodeB "DG" 
          [ NodeB "AC" [], NodeB "EE" [], NodeB "HK" []
          ]
       , NodeB "PU" 
          [ NodeB "MM" [], NodeB "RS" [], NodeB "UW" []
          ]
       ]

tBt2 :: Btree Char 
tBt2 = NodeB "GP"
       [ NodeB "ACDEE" [], NodeB "HKLMM" [], NodeB "RSUUW" []
       ]

tBt5 :: Btree Char 
tBt5 = NodeB "GMPX"
       [ NodeB "ACDE" [] , NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt6 :: Btree Char 
tBt6 = NodeB "GMPX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt7 :: Btree Char 
tBt7 = NodeB "GMPTX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
       ]

tBt8 :: Btree Char 
tBt8 = NodeB "P"
       [ NodeB "GM"
          [ NodeB "ABCDE" [], NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]

tBt9 :: Btree Char 
tBt9 = NodeB "P"
       [ NodeB "CGM"
          [ NodeB "AB" [], NodeB "DEF" []
          , NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]
