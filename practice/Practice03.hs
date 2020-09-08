{-# OPTIONS_GHC -Wall #-}
module Practice03 where

type Expr = [Term]
type Term = [String]

breaks :: [a] -> [[[a]]]
breaks [] = error "breaks"
breaks [v] = [[[v]]]
breaks (x:xs) = (map (\(ys:yss) -> [x] :(ys:yss)) (breaks xs)) ++ (map(\(ys:yss) -> (x:ys):yss) (breaks xs))

build :: String -> [Expr]
build ds = let xsss = breaks ds
               xsssss = map breaks xsss
           in concat xsssss

evalTerm :: Term -> Int
evalTerm xs = product (map read xs) 

eval :: Expr -> Int
eval ts = sum (map evalTerm ts)

find :: Int -> String ->[Expr]
find v ds = filter(\e -> eval e == v)(build ds)
-- find v = filter ((==v).eval) . build

showT :: Term -> String
showT fs = tail(concat (map ('*':) fs))

-- showE :: Expr -> String
-- showE ts = tail(concat(map("+":)))