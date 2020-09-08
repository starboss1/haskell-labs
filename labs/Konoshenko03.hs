{-# OPTIONS_GHC -Wall #-}
module Konoshenko03 where

type Algorithm    = [Substitution]
type Substitution = (String,String,Bool)
type ConfigA      = (Bool, Int, String)

data Command = Z Int | S Int | T Int Int | J Int Int Int deriving Show
type Program = [Command]
type ConfigC = (Int, Int, [Int])

-- Задача 1 ------------------------------------
isPrefix :: String -> String -> Bool
isPrefix "" _ = True
isPrefix _ "" = False
isPrefix (b:bs) (x:xs) = (b == x) && (isPrefix bs xs) 


replacePrefix :: String -> String -> String -> String
replacePrefix from to st = to ++ drop (length from) st

-- -- Задача 2 ------------------------------------
substitute :: Substitution -> Int -> String -> String
substitute sub i w = let t = drop i w
                         b = take i w
                     in if (isPrefix (subGet1th sub) t) then b ++ (replacePrefix (subGet1th sub) (subGet2nd sub) t) 
                         else w

-- Задача 3------------------------------------
findPosition :: String -> Substitution -> [(Substitution,Int)] 
findPosition w sub = [ (sub, x)| x <- [0..(length w)], w /= (substitute sub x w)]

-- Задача 4 ------------------------------------
findAll :: Algorithm -> String -> [(Substitution,Int)]  
findAll algo w = let xs = map (\e -> findPosition w e) algo
                 in concat xs


getSub :: Algorithm -> String -> (Substitution, Int)
getSub algo word = head (findAll algo word)
-- Задача 5 ------------------------------------
stepA :: Algorithm -> ConfigA -> ConfigA
stepA algo (bt, st, word) | bt == True = 
                            let s = getSub algo word
                             in ((not (subGet3th (fst s))), (st+1), (substitute (fst s) (snd s) word))
                          | otherwise =  (bt,(st+1),word)

-- Задача 6 ------------------------------------

evalA :: Algorithm -> Int -> String -> Maybe String 
evalA algo m word = let conf = (True, 0, word)
                        res = until cond step conf
                          where cond :: ConfigA -> Bool
                                cond (br,st,_) = (st >= m || br == False)
                                step :: ConfigA -> ConfigA
                                step c = stepA algo c
                      in if((conGet1th res) == True) then Nothing else Just (conGet3th res)


-- Задача 7 ------------------------------------
maximReg :: Program -> Int
maximReg [] = 0
maximReg pr = maximum (map getCommand pr) 

-- Задача 8 ------------------------------------
ini :: Program -> [Int] -> [Int] 
ini pr ir | ((maximReg pr) > (length ir)) = ir ++ [0 |_ <- [1..(maximReg pr - length ir)]]
          | otherwise = take (maximReg pr) ir

upd :: [Int] -> Int -> Int-> [Int]
upd reg r v | ((r < length reg) && (r >= 0)) =  take r reg ++ [v] ++ drop (r+1) reg
            | otherwise = error "Wrong index"

-- Задача 9 ------------------------------------
doCommand :: Command -> ConfigC -> ConfigC


doCommand (Z x) (nm, st, rg) = (nm+1, st+1, upd rg (x-1) 0) 
doCommand (S x) (nm, st, rg) = (nm+1, st+1,upd rg (x-1) ((rg !! (x-1))+1))
doCommand (T x y) (nm, st, rg) = (nm+1, st+1,upd rg (y-1) (rg !! (x-1)))
doCommand (J x y z) (nm, st, rg) | (rg !! (x-1)) == (rg !! (y-1)) = (z, st+1, rg)
                                 | otherwise = (nm+1, st+1, rg)


stepC :: Program -> ConfigC -> ConfigC
stepC pr (nm, st, rg) = doCommand (pr !! (nm-1)) (nm,st,rg)


-- Задача 10 ------------------------------------
evalC :: Program -> Int -> [Int] -> Maybe Int 
evalC pr mx ir = let conf = (1, 0, (ini pr ir ))
                     res = until cond step conf
                      where cond :: ConfigC -> Bool
                            cond (nm, st, _) = (st >= (mx+1) || nm > (length pr))
                            step :: ConfigC -> ConfigC
                            step c = stepC pr c
                  in if (conCGet2nd res) > mx then Nothing else Just (head(conCGet3th res))

subGet1th :: Substitution -> String
subGet1th (a,_,_) = a

subGet2nd :: Substitution -> String
subGet2nd (_,a,_) = a

subGet3th :: Substitution -> Bool
subGet3th (_,_,a) = a

conGet1th :: ConfigA -> Bool
conGet1th (a,_,_) = a

conGet2nd :: ConfigA -> Int
conGet2nd (_,a,_) = a

conGet3th :: ConfigA -> String
conGet3th (_,_,a) = a

conCGet1th :: ConfigC -> Int
conCGet1th (a,_,_) = a

conCGet2nd :: ConfigC -> Int
conCGet2nd (_,a,_) = a

conCGet3th :: ConfigC -> [Int]
conCGet3th (_,_,a) = a

getCommand :: Command -> Int
getCommand (Z x) = x
getCommand (S x) = x
getCommand (T x y) = max x y
getCommand (J x y _) = max x y

---------------------Тестові дані - Нормальні алгоритми Маркова ---------------------------
clearBeginOne, addEnd, reverse, multiply:: Algorithm 
-- стирає перший символ вхідного слова (алфавіт {a,b})
clearBeginOne = [ ("ca", "", True)
                , ("cb", "", True)
                , ("", "c", False)
                ] 

-- дописує abb в кінець вхідного слова (алфавіт {a,b})
addEnd = [ ("ca", "ac", False)
         , ("cb", "bc", False)
         , ("c", "abb", True)
         , ("", "c", False)
         ] 
-- зеркальне відображення вхідного слова (алфавіт {a,b})
reverse = [ ("cc", "d", False)
          , ("dc", "d", False)
          , ("da", "ad", False) 
          , ("db", "bd", False) 
          , ("d", "", True) 
          , ("caa", "aca", False) 
          , ("cab", "bca", False) 
          , ("cba", "acb", False)
          , ("cbb", "bcb", False) 
          , ("", "c", False) 
          ]

-- добуток натуральних чисел 
--  multiply ("|||#||") = "||||||"  3*2 = 6
multiply = [("a|", "|ba", False)
            ,("a", "", False)
            ,("b|", "|b", False)
            ,("|#", "#a", False)
            ,("#", "c", False)
            ,("c|", "c", False)
            ,("cb", "|c", False)
            ,("c", "", True)
            ]

---------------------Тестові дані - Програми МНР ---------------------------
notSignum, addition, subtraction :: Program 
-- функція notSignum x
notSignum = [Z 2, J 1 2 5, Z 1, J 1 1 6, S 1] 

-- функція додавання  addition x y = x+y
addition = [Z 3, J 3 2 6, S 1, S 3, J 1 1 2]

-- функція віднімання subtraction x y = x-y, визначена для x>=y 
subtraction = [Z 3, J 1 2 6, S 2, S 3, J 1 1 2, T 3 1]
