{-# OPTIONS_GHC -Wall #-}
module Konoshenko05 where

import Data.Char(isUpper)
import Data.List
import Data.Maybe

type Grammar    = [Production]         -- КВ-граматика
type Production = (Char,String)        -- Правило виводу
type Predict    = [(Char,String)]      -- Прогнозуюча таблиця
type Control    = [((Char,Char),Int)]  -- Управляюча таблиця 

-- Задача 1 ------------------------------------
addOne :: String -> Char -> String  
addOne st c =  if notElem c st then sort(st ++ [c]) else st

addAll :: String -> String -> String
addAll st (d:wd) = addAll (addOne st d) wd
addAll [] wd = wd
addAll st [] = st

addWithout :: String -> String -> String 
addWithout st wd= addAll st (filter (/='$') wd)

inter :: String -> String -> String 
inter st1 st2 = filter(\e -> (elem e st1 && elem e st2)) (addAll st1 st2)

-- Задача 2 ------------------------------------
tkPredict :: Predict -> Char -> String 
tkPredict pt n = concat (map snd (filter((==n) . fst)pt))


upPredict :: Predict -> Char -> String -> Predict 
upPredict (d:pt) n st | (fst d) == n = ((n,st):pt)
                      | (fst d) < n = [d] ++upPredict pt n st
                      | otherwise = ((n,st):d:pt)
upPredict [] n st = [(n,st)]

-- Задача 3 ------------------------------------
parse ::  Grammar -> Control -> String -> Maybe [Int]
parse gr ctl st = let startCh = (fst.head)gr
                      conf = (st++['$'], ([startCh]++"$"),Just [])
                      res = until cond stepP conf
                       where cond :: (String, String, Maybe [Int]) -> Bool
                             cond (ch1:_, ch2:_, result) = (ch1 == '$' && ch2 == '$') ||isNothing result
                             cond ([], _, _) = True
                             cond ((_:_), [], _) = True
                             stepP :: (String, String, Maybe [Int]) -> (String, String, Maybe [Int])
                             stepP c = step gr ctl c
                  in if((get2nd res ) !! 0 )/= '$' then Nothing else get3th res

getContr :: Control -> Char -> Char -> Maybe Int
getContr ctl inCh stCh = let res = (filter (\e -> (stCh == (fst(fst e)) && (inCh == (snd(fst e))))) ctl)
                         in if null res then Nothing else Just ((snd .head)res)

step :: Grammar -> Control -> 
       (String, String, Maybe [Int]) -> (String, String, Maybe [Int])
step gr ctl (ch1:input, ch2:stack, result) 
    | (isUpper ch2) = case getContr ctl ch1 ch2 of
        Just n -> (ch1:input,snd(gr !! n) ++ stack,(++[n]) <$> result)
        Nothing -> (ch1:input, ch2:stack, Nothing)
    | ch1 == ch2 = (input,stack,result)
    | otherwise = (ch1:input, ch2:stack,Nothing)
step _ _ (_, _, _) = ("","",Nothing)

get1th :: (String, String, Maybe [Int]) -> String
get1th (a,_,_) = a

get2nd :: (String, String, Maybe [Int]) -> String
get2nd (_,a,_) = a

get3th :: (String, String, Maybe [Int]) -> Maybe [Int]
get3th (_,_,a) = a

-- Задача 4 ------------------------------------
first :: Predict -> String -> String
first pFst (ch1:ch2:st) = 
  case isUpper ch1 of
    True -> if elem '$' (tkPredict pFst ch1) then addWithout (first pFst (ch2:st))(tkPredict pFst ch1) else (tkPredict pFst ch1)
    False -> [ch1]
first pFst (ch1:_) = if isUpper ch1 then (tkPredict pFst ch1) else [ch1]

first _ "" = "$"

-- Задача 5 ------------------------------------

temp :: Production -> Predict -> Predict  -> [(Char, Char)]
temp (ter,nter) pFst pNxt = let firstRes = first pFst nter
                              in  if null firstRes || (elem '$' firstRes && (length firstRes) == 1)
                                then map(\x -> (ter,x))(tkPredict pNxt ter)
                                else map(\y -> (ter,y))firstRes
comp :: ((Char, Char),Int) -> ((Char, Char),Int) -> Ordering
comp a b | ((fst .fst) a) < ((fst . fst) b) = LT
         | ((fst .fst) a) == ((fst . fst) b) && ((snd . fst) a) < ((snd . fst) b) = LT
         | otherwise = GT

buildingControlRec:: Grammar -> Predict -> Predict -> Int -> Control
buildingControlRec (g:gr) pFst pNxt n = ((map (\q -> (q,n))(temp g pFst pNxt)) ++ buildingControlRec gr pFst pNxt (n+1))
buildingControlRec _ _ _ _ = []

buildingControl :: Grammar -> Predict -> Predict -> Control
buildingControl gr pFst pNxt = sortBy comp (buildingControlRec gr pFst pNxt 0)

-- Задача 6 ------------------------------------
doFirst :: Predict -> [(Char,[String])] -> [(Char,[String])]
doFirst pFst s = map (\(ch,es) -> (ch,map (first pFst)es)) s
testingLL1 :: Grammar -> Predict -> Predict -> Bool
testingLL1  gr pFst pNxt = let fgs = fromGrammar gr
                               res = filter (\e -> if 
                                (testFst (map (first pFst) (snd e))) && 
                                (if elem "" (snd e) then testFollow (tkPredict pNxt (fst e))  (snd e) else True)
                                then False else True) fgs
                            
                           in if null res then True else False

concatTuple :: [(Char, String)] -> (Char, [String])
concatTuple ts = ((fst.head) ts, foldl (\x y -> x ++ [(snd y)]) [] ts)

fromGrammar :: Grammar ->  [(Char,[String])]
fromGrammar gr = map concatTuple (groupBy(\a b -> fst a == fst b) gr)

testFst :: [String] -> Bool
testFst (r:rls) = if null (filter (\x -> (not. null)(intersect r x)) rls) then testFst rls else False
testFst _ = True

testFollow :: String -> [String] -> Bool
testFollow fs rls = if null (filter (\x -> (not. null)(intersect fs x))rls) then True else False

-- Задача 7 ------------------------------------
tkGrammar :: Grammar -> Char -> [String]
tkGrammar pt n = map snd (filter((==n) . fst)pt)

fstHandl :: Grammar -> Predict -> Predict
fstHandl gr pFst | pFst == nextPFst = pFst
                 | otherwise = fstHandl gr nextPFst 
                 where 
                  nextPFst = evalFst gr pFst

initPredictFst :: Grammar -> Predict -> Predict 
initPredictFst ((ch,output):gr) prdct |null output = initPredictFst gr (upPredict prdct ch "$")
                                      | otherwise = initPredictFst gr (upPredict prdct ch (addAll (tkPredict prdct ch) ""))
initPredictFst _ p = p

buildFst :: Grammar -> Predict 
buildFst gr = let i = initPredictFst gr []
              in fstHandl gr i

evalFst :: Grammar -> Predict -> Predict
evalFst (p:gr) pFst = evalFst gr (extandFst pFst p)
evalFst _ pFst = pFst


extandFst :: Predict -> Production -> Predict 
extandFst pFst (n,nul) = upPredict pFst n (addAll (first pFst (nul)) (tkPredict pFst n))

-- Задача 8 ------------------------------------

nxtHandl :: Grammar -> Predict -> Predict -> Predict
nxtHandl tailList pFst pNxt | pNxt == nextPNxt = pNxt
                            | otherwise = nxtHandl tailList pFst nextPNxt 
                            where 
                             nextPNxt = evalNxt tailList pFst pNxt

buildNxt :: Grammar -> Predict -> Predict 
buildNxt gr pFst = let startCh = (fst.head)gr
                       i = initPredictNxt gr [] startCh
                       tailList = nontermTails gr
                   in nxtHandl tailList pFst i




evalNxt :: [(Char,String)] -> Predict -> Predict -> Predict
evalNxt ((ch,output):tailss) pFst pNxt = evalNxt tailss pFst (extandNxtOne pFst ch pNxt output)
evalNxt _ _ pNxt = pNxt

extandNxtOne :: Predict -> Char -> Predict -> String -> Predict
extandNxtOne pFst n pNxt (m:st)  = let firstB = (first pFst st)
                                       nxtY   = tkPredict pNxt m
                                       nxtA   = tkPredict pNxt n
                                   
                                   in if elem '$' firstB then upPredict pNxt m (addAll (addWithout nxtY firstB ) (nxtA))
                                    else upPredict pNxt m (addAll nxtY firstB)

extandNxtOne pFst n pNxt m       = let firstB = (first pFst "")
                                       nxtY   = tkPredict pNxt ( head m)
                                       nxtA   = tkPredict pNxt n
                                   in if elem '$' firstB then upPredict pNxt (head m) (addAll (addWithout nxtY firstB ) (nxtA))
                                    else upPredict pNxt (head m) (addAll nxtY firstB)

initPredictNxt :: Grammar -> Predict -> Char -> Predict 
initPredictNxt ((ch,_):gr) prdct startCh |startCh == ch = initPredictNxt gr (upPredict prdct ch "$") startCh
                                              | otherwise = initPredictNxt gr (upPredict prdct ch (addAll (tkPredict prdct ch) "")) startCh
initPredictNxt _ p _ = p


initsProduct:: (Char, String)-> [(Char, String)]
initsProduct (ch,output) = reverse $map (\e -> (ch,e)) (filter (\x-> ((length x)/= 0) && isUpper (head x))(map (reverse)((inits. reverse)output)))

nontermTails :: Grammar -> [(Char,String)] 
nontermTails gr = nubBy (\(x1,y1) (x2,y2) -> (x1 == x2 && y1 == y2))(concatMap initsProduct gr)

compareTuple :: (Char, String) -> (Char, String) -> Ordering
compareTuple a b | (fst a) < (fst b) = LT
                 | (fst a) == (fst b) && (snd a) < (snd b) = LT
                 | otherwise = GT

---------------------Тестові дані ---------------------------
gr0, gr1, gr2, gr3, gr4, gr5:: Grammar
--  LL(1)-граматики
gr0 = [('S',"aAS"),('S',"b"), ('A',"a"), ('A',"bSA")]  
gr1 = [('S',"TV"),('T',"d"),('T',"(S)"),('V',"+TV"),('V',"-TV"),('V',"")]  
gr2 = [('E',"TU"),('U',""),('U',"+TU"),('U',"-TU"),
       ('T',"FV"),('V',""),('V',"*FV"),('V',"%FV"),('V',"/FV"),
       ('F',"d"),('F',"(E)")] 
-- не LL(1)-граматики
gr3 = [('S',"aAS"), ('S',"a"),('A',"SbA"),('A',"ba"),('S',"")]
gr4 = [('E',"E+T"),('E',"T"), ('T',"T*F"), ('T',"F"), ('F',"d"),('F',"(E)") ]   
gr5 = [('E',"E+T"), ('E',"E-T"),('E',"T"), 
       ('T',"T*F"), ('T',"T%F"), ('T',"T/F"), ('T',"F"), 
       ('F',"d"),('F',"(E)") ]

-- прогнозуючі таблиці початкових терміналів Fst
pFst0, pFst1, pFst2, pFst3, pFst4, pFst5 :: Predict
pFst0 = [('A',"ab"),('S',"ab")]
pFst1 = [('S',"(d"),('T',"(d"),('V',"$+-")]
pFst2 = [('E',"(d"),('F',"(d"),('T',"(d"),('U',"$+-"),('V',"$%*/")]
pFst3 = [('A',"ab"),('S',"$a")]
pFst4 = [('E',"(d"),('F',"(d"),('T',"(d")]
pFst5 = [('E',"(d"),('F',"(d"),('T',"(d")]

-- прогнозуючі таблиці наступних терміналів Nxt
pNxt0, pNxt1, pNxt2, pNxt3, pNxt4, pNxt5 :: Predict
pNxt0 = [('A',"ab"),('S',"$ab")]
pNxt1 = [('S',"$)"),('T',"$)+-"),('V',"$)")]
pNxt2 = [('E',"$)"),('F',"$%)*+-/"),('T',"$)+-"),('U',"$)"),('V',"$)+-")]
pNxt3 = [('A',"$ab"),('S',"$b")]
pNxt4 = [('E',"$)+"),('F',"$)*+"),('T',"$)*+")]
pNxt5 = [('E',"$)+-"),('F',"$%)*+-/"),('T',"$%)*+-/")]   

-- управляючі таблиці 
ctl0, ctl1, ctl2 :: Control 
ctl0 = [(('A','a'),2),(('A','b'),3),(('S','a'),0),(('S','b'),1)]
ctl1 = [(('S','('),0),(('S','d'),0),(('T','('),2),(('T','d'),1),
        (('V','$'),5),(('V',')'),5),(('V','+'),3),(('V','-'),4)]
ctl2 = [(('E','('),0),(('E','d'),0),(('F','('),10),(('F','d'),9),
        (('T','('),4),(('T','d'),4),(('U','$'),1),(('U',')'),1),
        (('U','+'),2),(('U','-'),3),(('V','$'),5),(('V','%'),7),
        (('V',')'),5),(('V','*'),6),(('V','+'),5),(('V','-'),5),(('V','/'),8)]

