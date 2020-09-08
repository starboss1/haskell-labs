{-# OPTIONS_GHC -Wall #-}
module Konoshenko08 where

import Text.ParserCombinators.Parsec
import Data.Char(isSpace)

data Recur = Zero | Succ | Sel Int Int 
           | Super Recur [Recur] 
           | Prim Recur Recur 
           | Mini Recur Int 
           | Name String  deriving (Show, Eq)
type System = [(String,Recur)]  

-- Задача 1 -----------------------------------------

isNumbConst :: System -> Recur -> Bool
isNumbConst _ Zero = True
isNumbConst syst rec@(Super Succ f) = isNumbConst syst (head f) && evRank syst (rec) ==1
isNumbConst syst rec@(Super Zero f) = isNumbConst syst (head f) && evRank syst (rec) ==1
isNumbConst _ (Sel _ _) = True
isNumbConst syst (Name name) = let funct = getFunct syst name
                               in    isNumbConst syst funct
isNumbConst _ _ = False
                               

-- Задача 2 -----------------------------------------
evRank :: System -> Recur -> Int 
evRank _ Zero = 1
evRank _ Succ = 1
evRank _ (Sel n _) = n
evRank syst (Super _ al) = evRank syst (head al)
evRank syst (Prim _ st) = (evRank syst st) - 1
evRank syst (Mini b _) = (evRank syst b) - 1
evRank syst (Name f) = evRank syst (getFunct syst f)

-- Задача 3 -----------------------------------------
isNames :: System -> Bool 
isNames syst = isNamesRec syst []

isNamesRec :: System -> [String] -> Bool
isNamesRec (s:syst) names = let namesInFunct = getNames (snd s)
                                correctNames = and (map (\e -> elem e names)namesInFunct)
                            in if correctNames then isNamesRec syst (names ++ [(fst s)]) else False
isNamesRec [] _ = True

-- Задача 4 -----------------------------------------
isRecur :: System -> Recur -> Bool
isRecur syst f = correctFunctNames syst f && isCorrectSignature syst f


isCorrectSignature :: System -> Recur  -> Bool
isCorrectSignature _ (Name _) = True
isCorrectSignature _ Zero = True
isCorrectSignature _ Succ= True
isCorrectSignature _ (Sel i j) = i >= j
isCorrectSignature syst (Super b al) = 
  isCorrectSignature syst b && and (map (\e -> isCorrectSignature syst e) al) 
  && allTheSame ( map(evRank syst)al)

isCorrectSignature syst (Prim i st) = 
  (isCorrectSignature syst i) && (isCorrectSignature syst st) && correctRank
  where correctRank = if(isNumbConst syst i) then (elem ((evRank syst st)- (evRank syst i)) [1,2])
                       else ((evRank syst st)- (evRank syst i) == 2) 

isCorrectSignature syst m@(Mini b t) = (isCorrectSignature syst b) && (t >=0) && evRank syst m >1

-- Задача 5 -----------------------------------------
isCorrectArgs :: System -> Recur -> [Int] -> Bool
isCorrectArgs syst f vl = isCorrectSignature syst f && (evRank syst f) == (length vl)

eval :: System -> Recur -> [Int] -> Int
eval syst sup@(Super b al) vl | isCorrectArgs syst sup vl = eval syst b (map (\e -> eval syst e vl)al)
                              | otherwise = error "Wrong number of args"
eval _ Zero _ =  0 
eval _ Succ vl = (head vl) + 1 
eval _ (Sel _ j) vl =vl !! (j-1) 
eval syst (Name name) vl = eval syst (getFunct syst name) vl
eval syst pr@(Prim i st) vl = if isCorrectArgs syst pr vl then 
                                last(until (cond (last vl)) (step syst st) start)
                                else error "Wrong number of args"
                               where first = (eval syst i vl)
                                     start = buidPrimArgs syst (Prim i st)((take 1 vl)++[0]++[first])
eval _ (Mini _ _) _ = undefined           

buidPrimArgs :: System -> Recur -> [Int] -> [Int]
buidPrimArgs syst (Prim _ st) vl = let rank = evRank syst st
                                    in drop (length vl - rank) vl 
buidPrimArgs _ _ _ = []

cond :: Int -> [Int] -> Bool
cond s star = (star !! ((length star)-2)) >= s

step :: System -> Recur -> [Int] -> [Int]
step syst st vl = (take ((length vl)-2) vl) ++ [(vl !! ((length vl)-2))+1] ++[eval syst st vl]


-- Задача 6 -----------------------------------------
evalPart :: System -> Recur -> [Int] -> Maybe Int
evalPart syst (Name name) vl = evalPart syst (getFunct syst name) vl
evalPart syst (Mini f i) vl = let res = filter (\e -> 0==(eval syst f (vl ++ [e])))[0..i]
                                 in if null res then Nothing else Just (head res)
evalPart syst f vl = Just (eval syst f vl)
-- Задача 7 -----------------------------------------

parseRec :: String -> Maybe System 
parseRec str = case parse system "" filtrStr of
                    Left _ -> Nothing
                    Right res -> Just res
               where filtrStr = filter (not . isSpace) str

integer :: Parser Int
integer = do digits <- many1 digit
             return $ read digits

iden :: Parser Recur
iden = do a <- letter
          b <- many (digit <|> letter)
          return (Name (a:b))

z1, a1, sel :: Parser Recur
z1 = do _ <- string "z1"
        return Zero

a1 = do _ <- string "a1"
        return Succ

sel = do _ <- char 's'
         i <- digit
         j <- digit
         return (Sel (read [i]) (read [j]))

recur :: Parser Recur
recur = base <|> super <|> prim <|> mini

base :: Parser Recur
base = try a1 <|> try z1 <|> try sel <|> iden

super :: Parser Recur
super = do _ <- char '('
           recur1 <- recur
           _ <- char ':'
           recur2 <- recur
           recur3 <- many (do {_ <- char ','; recur4 <- recur; return recur4})
           _ <- char ')'
           return (Super recur1 (recur2:recur3))

prim :: Parser Recur
prim = do _ <- char '['
          recur1 <- recur
          _ <- char ','
          recur2 <- recur
          _ <- char ']'
          return (Prim recur1 recur2)

mini :: Parser Recur
mini = do _<- char '{'
          recur1 <- recur
          _ <- char ','
          recur2 <- integer
          _ <- char '}'
          return (Mini recur1 recur2) 

system :: Parser System
system = do sysOp <- many (do {ide <- idenNoName; _ <- char '='; recur1 <- recur; _ <- char ';'; return (ide,recur1)})
            eof
            return sysOp

idenNoName :: Parser String
idenNoName = do a<-letter
                b <- many (digit <|> letter)
                return (a:b)

getFunct :: System -> String -> Recur
getFunct syst name = let res = filter ((==name).fst)syst
                      in (snd.head) res

getNames :: Recur -> [String]
getNames (Name name) = [name]
getNames (Super b al) = (getNames b) ++ (concatMap (getNames)al)
getNames (Prim i st) = (getNames i) ++ (getNames st)
getNames (Mini b _) = getNames b
getNames _ = []

-- Отримати назви всіх функцій в системі
getAllNamesInSyst :: System -> [String]
getAllNamesInSyst syst = map (fst)syst

-- Правильна назва функцій в виразі
correctFunctNames :: System -> Recur -> Bool
correctFunctNames syst f = let namesInSystem = getAllNamesInSyst syst
                               namesInRecur = getNames f
                            in and (map (\e -> elem e namesInSystem)namesInRecur)

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

---------------------Òåñòîâ³ äàí³ -  -------
syst1, syst2 :: System 
syst1 = [("const0", Zero)   
   , ("const0v2", Super Zero [Sel 2 1])
   , ("const0v3", Super Zero [Sel 3 1])
   , ("const1v2", Super Succ [Super Zero [Sel 2 1]]) 
   , ("const2", Super Succ [Super Succ [Zero]]) 
   , ("addition", Prim (Sel 1 1) (Super Succ [Sel 3 3 ])) 
   , ("multiplication", Prim Zero (Super (Name "addition") [Sel 3 3, Sel 3 1]))  
   , ("notSignum", Prim (Super Succ [Zero]) (Super Zero [Sel 2 1]))  
   , ("subtract1", Prim Zero (Sel 2 1))  
   , ("subtraction", Prim (Sel 1 1) (Super (Name "subtract1") [Sel 3 3]))  
   , ("subtractionRev", Super (Name "subtraction") [Sel 2 2, Sel 2 1])     
   , ("subtractionAbs", Super (Name "addition") [Name "subtraction", Name "subtractionRev"])  
   , ("subtractionAbs3", Super (Name "subtractionAbs") [Sel 3 1, Super (Name "addition") [Sel 3 2, Sel 3 3]])  
   , ("subtractionPart", Mini (Name "subtractionAbs3") 100)    
   ]
   
syst2 = [("f1", Super Succ [Zero])
        ,("f2", Super Succ [Name "f2"])
        ]


sysStr1,sysStr2 :: String    
sysStr1 = " const0 = z1; const0v2  = (z1 : s21); const0v3 = (z1:s31);\n\
          \  const1v2 = (a1 : (z1 : s21));  \n\
          \  const2= (a1:(a1:z1)); addition = [s11, (a1:s33)] ;\n\
          \  multiplication = [z1 , (addition: s33,s31)]; \n\
        \  notSignum = [(a1:z1),(z1:s21)];\n\
      \  subtract1 = [z1,s21]; subtraction = [s11, (subtract1:s33)];\n\
      \  subtractionRev = (subtraction : s22, s21);\n\
          \  subtractionAbs = (addition: subtraction, subtractionRev); \n\
          \  subtractionAbs3=(subtractionAbs:s31, (addition:s32,s33))  ;\n \
          \ subtractionPart = {subtractionAbs3, 100 };"
 
sysStr2 = " f1 = (a1:z1); f2 = (a1, f2);"
