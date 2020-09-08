{-# OPTIONS_GHC -Wall #-}
module Konoshenko10 where

import Data.List
import qualified Text.ParserCombinators.Parsec as P

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition = (State, State, Label)
type Automation = (State, [State], [Transition])

type MetaState = [State]
type MetaTransition = (MetaState, MetaState, Label)

-- Задача 1 -----------------------------------------
simplify :: RE -> RE   
simplify (Null) = Null
simplify (Term ch) = Term ch
simplify (Seq reA reB) = (Seq (simplify reA) (simplify reB))
simplify (Alt reA reB) = (Alt (simplify reA) (simplify reB))
simplify (Rep reA) = Rep (simplify reA)
simplify (Plus reA) = Seq (simplify reA) (Rep (simplify reA))
simplify (Opt reA) = Alt (simplify reA) Null

-- Задача 2 -----------------------------------------
isTerminal :: Automation -> State -> Bool 
isTerminal (_,finStates,_) s = elem s finStates

isEssential :: Automation -> State -> Bool 
isEssential aut s = 
  let haveTerm = haveCharTrans (transitionsFrom aut s)
  in isTerminal aut s || (not . null) haveTerm

-- Задача 3 -----------------------------------------
transitionsFrom :: Automation -> State -> [Transition]
transitionsFrom (_,_,transit) s = filter (\(state,_,_) -> (s == state)) transit

-- Задача 4 -----------------------------------------
labels :: [Transition] -> [Label]
labels trx = 
  let allLabels = map (label) (haveCharTrans trx)
  in nub allLabels

-- Задача 5 -----------------------------------------
acceptsDA :: Automation -> String -> Bool
acceptsDA daut@(start, _, _) st = acceptsDARec daut start st

acceptsDARec :: Automation -> State -> String -> Bool
acceptsDARec daut state [] = isTerminal daut state
acceptsDARec daut@(_, _, _) state (s:ss)  
  | isTerminal daut state = False 
  | otherwise = if null res then False else acceptsDARec daut (nextState(head res)) ss 
    where transFrom = transitionsFrom daut state
          res = filter (\(_,_,l) -> (C s) ==l)transFrom


-- Задача 6 -----------------------------------------
stStep  :: Automation -> State -> Label -> [State]
setStep :: Automation -> [State] -> Label -> [State]
closure :: Automation -> [State] -> [State]

stStep naut st mc = map (nextState) $ filter (\(_,_,l) -> l == mc)(transitionsFrom naut st)
setStep naut bs mc = concatMap (\st -> stStep naut st mc) bs
closure naut ss = (sort . nub . (++ ss)) (closureRec naut ss [])

closureRec :: Automation -> [State] -> [State] -> [State]
closureRec _ [] _ = []
closureRec naut ss visited = 
  let st = setStep naut ss Eps
      notVisitedNextStates = filter (\e -> notElem e visited)st
  in st ++ (closureRec naut notVisitedNextStates (st++visited))
                           

-- Задача 7 -----------------------------------------
accepts :: Automation -> String -> Bool
accepts aut@(start,_,_) st = acceptsRec aut st [start]

acceptsRec :: Automation -> String -> [State] -> Bool
acceptsRec aut [] state = any (\e -> isTerminal aut e)state
acceptsRec _ _ [] = False
acceptsRec aut (s:st) state = let allNxt = closure aut state
                                  nxt = setStep aut allNxt (C s)
                              in acceptsRec aut st nxt

-- Задача 8 -----------------------------------------
makeNDA :: RE -> Automation
makeNDA re = (1, [2], sort transitionsAll)
  where (transitionsAll, _) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int) 
make Null beg fin nxt = ([(beg,fin, Eps)],nxt)
make (Term ch) beg fin nxt = ([(beg,fin, C ch)], nxt)
make (Seq reA reB) beg fin nxt = 
  let (rA1, rA2) = make reA beg nxt (nxt + 2)
      (rB1,rB2) = make reB (nxt + 1) fin rA2
  in (rA1 ++ [(nxt, nxt + 1, Eps)] ++ rB1, rB2)


make (Rep rA) beg fin nxt = 
  let (r1, r2) = make rA nxt (nxt + 1) (nxt + 2)
  in ([(beg, fin, Eps)] ++ [(beg, nxt, Eps)] ++ r1 ++ [((nxt +1), nxt, Eps)] ++ [(nxt + 1, fin, Eps)], r2)

make (Alt rA rB) beg fin nxt =
  let (rA1, rA2) = make rA nxt (nxt + 1) (nxt + 4)
      (rB1, rB2) = make rB (nxt + 2) (nxt + 3) (rA2)
  in ([(beg, nxt, Eps)] ++ rA1 ++ [(nxt + 1, fin, Eps)] ++ [(beg, nxt + 2, Eps)] ++ rB1 ++ [(nxt + 3, fin, Eps)], rB2)

make (Plus _) _ _ _ = error "error plus"
make (Opt _) _ _ _ = error "error opt"

-- Задача 9 -----------------------------------------
parseReg :: String -> Maybe RE 
parseReg str = case P.parse reg "" str of
                    Left _ -> Nothing
                    Right res -> Just res

rsymb :: (P.Parser) RE
rsymb = do x <- P.noneOf "()|*+?"
           return (Term x)

prime :: P.Parser RE
prime = rsymb P.<|> do {_ <- P.char '('; r <- rexpr; _ <- P.char ')'; return r}

rfact :: P.Parser RE
rfact = do x <- prime
           op <- P.many (P.oneOf "*?+")
           return $ createOp x (reverse op)

createOp :: RE -> String -> RE
createOp x [] = x
createOp x (y:ys) | y == '*' = Rep (createOp x ys)
                  | y == '+' = Plus (createOp x ys)
                  | y == '?' = Opt (createOp x ys)
createOp _ _ = Null


rterm :: P.Parser RE
rterm = do x <- rfact
           y <- P.many rfact

           return (if null y then x else createSeq (x:y))

createSeq :: [RE] -> RE
createSeq (x:[]) = x
createSeq (x:xs) = Seq x (createSeq xs)
createSeq [] = Null

rexpr :: P.Parser RE
rexpr = do x <- rterm
           y <- P.many (do _ <- P.char '|'; a <- rfact; return a)
           return (if null y then x else createAlt (x:y))

createAlt :: [RE] -> RE
createAlt (x:[]) = x
createAlt (x:xs) = Alt x (createAlt xs)
createAlt [] = Null

reg :: P.Parser RE
reg = do r <- rexpr
         P.eof
         return r

-- Задача 10 -----------------------------------------

makeDA :: Automation -> Automation
makeDA aut@(_,finish,_) = 
  let (_,mstx,mtrx) = makeDA' aut
      finishStates = [x + 1 | x <- [0..length mstx-1], (not . null) ( intersect (mstx !! x) finish)]
      trans = map (\(a,b,c) -> (indexMetaState mstx a, indexMetaState mstx b, c)) mtrx
  in (1, finishStates, (sortBy comparator trans))

makeDA' :: Automation -> (MetaState, [MetaState], [MetaTransition])
makeDA' aut@(start, _, _) = 
  let cl = filter (\e -> (isEssential aut e)) (closure aut [start])
      (st,_,t) =  until cond (step aut)([],[cl],[])
  in (head st,st,t)


cond :: ([MetaState], [MetaState], [MetaTransition]) -> Bool
cond (_,a,_) = null a

step :: Automation -> ([MetaState], [MetaState], [MetaTransition]) -> ([MetaState], [MetaState], [MetaTransition])
step aut (gmsx,(msx:bmsx),mtrx) = 
  let l = nub $ concatMap (labels.(transitionsFrom aut)) msx
      mlx = map ((closure aut).(setStep aut msx))l
      shortMlx = map (\e -> filter (isEssential aut) e) mlx
  in addStates (gmsx ++ [msx], bmsx, mtrx) msx shortMlx l
step _ a@(_, [], _) = a


addStates :: ([MetaState], [MetaState], [MetaTransition]) 
              -> MetaState -> [MetaState] -> [Label] -> ([MetaState], [MetaState], [MetaTransition])
addStates a@(gmsx,bmsx,mtrx) msx mlx l = 
  let newBmsx = if elem (head mlx) bmsx || elem (head mlx) gmsx then bmsx else bmsx ++ [(head mlx)]
      newMtrx = mtrx ++ [(msx, (head mlx), head l)]
  in if null mlx then a else addStates (gmsx, newBmsx ,newMtrx) msx (tail mlx) (tail l) 
          

indexMetaState :: [MetaState] -> MetaState -> Int
indexMetaState ls i = 
  case elemIndex i ls of
    Just t -> t + 1
    Nothing -> error "error"

comparator :: (Int, Int, Label) -> (Int, Int, Label) -> Ordering
comparator (a1, b1, _) (a2, b2, _)
  | a1 > a2 = GT
  | a1 < a2 = LT
  | otherwise = compare b1 b2

haveCharTrans :: [Transition] -> [Transition]
haveCharTrans tr = filter (\(_,_, l) -> l /= Eps) tr

startState     :: Automation -> State
startState (start, _, _) = start

finalStates :: Automation -> [State]
finalStates (_, finStates, _) = finStates

transitions    :: Automation -> [Transition]
transitions (_, _, tr) = tr

label :: Transition -> Label
label (_,_,l) = l

nextState :: Transition -> State
nextState (_,st,_) = st

-------------------------------------------------------
-- showRE - Функція може бути корисною при тестуванні
showRE :: RE -> String
showRE (Seq re re') = showRE re ++ showRE re'
showRE (Alt re re') = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)     = showRE' re ++ "*"
showRE (Plus re)    = showRE' re ++ "+"
showRE (Opt re)     =  showRE' re ++ "?"
showRE re           = showRE' re

showRE' :: RE -> String
showRE' Null      = ""
showRE' (Term c)  = [c]
showRE' (Alt re re') = showRE (Alt re re')
showRE' re        = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Тестові приклади
reFigureS, re1S, re2S, re3S, re4S, re5S, re6S :: String
reFigureS = "(a|b)*c"
re1S = "(x|y)(1|2)"
re2S = "x'*"
re3S = "(ab|c)*"
re4S = "(a?)a"
re5S = "(ab)?d+"
re6S = "c?*"

reFigure, re1, re2, re3, re4, re5, re6 :: RE
reFigure = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1 = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2 = Seq (Term 'x') (Rep (Term '\''))
re3 = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4 = Seq (Opt(Term 'a')) (Term 'a')
re5 = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))
re6 = Rep (Opt (Term 'c'))

ndaFigure, nda1, nda2, nda3, nda4, nda5, nda6, ndaTest :: Automation
daFigure, da1, da2, da3, da4, da5, da6 :: Automation
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],[(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1 = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2 = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2 = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3 = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3 = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4 = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5 = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

nda6 = (1,[2], [(1,2,Eps),(1,3,Eps),(3,5,Eps),(5,6, C 'c'), (6,4,Eps), 
                (4,2,Eps), (3,7,Eps), (7,8,Eps), (8,4,Eps), (4,3,Eps)]) 
da6 = (1,[1], [(1,1, C 'c')])

ndaTest = (1, [1], [(1,2, C 'a'), (1,4, Eps), (1,3, C 'b'), (2,3, Eps),
              (3,5, Eps), (3,4, C 'a'), (4,4, Eps), (4,1, Eps), (5,2, Eps), (5,4,Eps)] )
