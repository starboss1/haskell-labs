{-# OPTIONS_GHC -Wall #-}
module Konoshenko09 where

-- розглядаємо лише цілі дані: скаляри  і масиви  
--------------------------------------------------------------------
import Data.List(nub)
type Id    = String
data Value = I Int | A [(Int, Int)]  deriving (Eq, Show)
data Op    = Add | Minus | Mul | Less | Equal | Index  deriving (Eq, Show)

data Exp = Const Int 
         | Var Id 
         | OpApp Op Exp Exp 
         | Cond Exp Exp Exp 
         | FunApp Id [Exp] 
         deriving (Eq, Show)

data Stmt = Assign Id Exp 
          | AssignA Id Exp Exp 
          | If Exp Stmt Stmt 
          | While Exp Stmt 
          | Call Id [Exp] 
          | Block [VarDef] [Stmt]
          deriving (Eq, Show)

data VarDef  =  Arr Id | Int Id deriving (Eq, Show)

type FunDef  =  (Id, ([VarDef], Exp))
-- функції повертають лише цілі скалярні дані, не використовують глобальні дані (чисті!!)
type ProcDef = (Id, ([VarDef], Stmt))
type Program = ([VarDef], [FunDef], [ProcDef])

type StateP  = [(Id, Value)]  -- стек даних

data Type    = At | It  deriving (Eq, Show)
type FunEnv  = [(Id,[Type])]
type ProcEnv = [(Id,[Type])]
type VarEnv  = [(Id,Type)]

-- Задача 1 ------------------------------------
updateValue :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
updateValue a b ((al,bl):abc) = if a == al then ((a,b):abc) else (al,bl):(updateValue a b abc)
updateValue a b [] = [(a,b)]

-- Задача 2 ------------------------------------
updateArray :: Value -> Value -> Value -> Value
updateArray (A a) (I i) (I v) = A (updateValue i v a)
updateArray _ _ _ = error "Wrong params"

-- Задача 3 ------------------------------------
applyOp :: Op -> Value -> Value -> Value 
applyOp Add (I v1) (I v2) = I (v1 + v2)
applyOp Minus (I v1) (I v2) = I (v1 - v2)
applyOp Mul (I v1) (I v2) = I (v1 * v2)
applyOp Less (I v1) (I v2) = if v1 < v2 then (I 1) else (I 0)
applyOp Equal (I v1) (I v2) = if v1 == v2 then (I 1) else (I 0)
applyOp Index (A arr) (I v) = if containsKey v arr then I (lookUp v arr) else (I 0)
applyOp _ _ _ = error "Wrong"

-- Задача 4 ------------------------------------
evExp ::  Exp -> [FunDef] -> StateP -> Value
evExp (Const e) _ _ = I e
evExp (Var e) _ st = lookUp e st
evExp (Cond e1 e2 e3) dfx st = if (evExp e1 dfx st) /= (I 0) 
                               then (evExp e2 dfx st) else (evExp e3 dfx st)
evExp (OpApp o e1 e2) dfx st = applyOp o (evExp e1 dfx st) (evExp e2 dfx st)
evExp (FunApp i exps) dfx st = let functDef = lookUp i dfx
                                   as = fst functDef
                                   ef = snd functDef
                                   vs = evArgs exps dfx st
                                   new = createState as vs
                               in evExp ef dfx new

createState :: [VarDef] -> [Value] -> StateP
createState ((Int name):vars) (val:vs) = [(name, val)] ++ createState vars vs
createState ((Arr name):vars) (val:vs) = [(name, val)] ++ createState vars vs
createState _ _ = []
 

evArgs :: [Exp] -> [FunDef] ->StateP -> [Value]  
evArgs ex dfx st = map (\e -> evExp e dfx st) ex

-- Задача 5 ------------------------------------
evStmt :: Stmt -> [FunDef] -> [ProcDef] -> StateP -> StateP
evStmt (Assign i exp1) dfx _ st = updateValue i (evExp exp1 dfx st) st
evStmt (AssignA i exp1 exp2) dfx _ st = let oldArray = lookUp i st 
                                            updArray = updateArray oldArray (evExp exp1 dfx st) (evExp exp2 dfx st)
                                        in updateValue i updArray st
evStmt (If exp1 stmt1 stmt2) dfx dpx st = if (evExp exp1 dfx st) /= (I 0) 
                                          then evStmt stmt1 dfx dpx st else evStmt stmt2 dfx dpx st
evStmt w@(While exp1 stmt1) dfx dpx st = if (evExp exp1 dfx st) /= (I 0) 
                                         then evStmt w dfx dpx (evStmt stmt1 dfx dpx st) else st
evStmt (Call i expList) dfx dpx st = 
  let params = map (\e -> evExp e dfx st) expList
      functDef = lookUp i dpx
      functParamName = fst functDef
      functBody = snd functDef
      newState = st ++ createState functParamName params
      res = evStmt functBody dfx dpx newState
  in take (length res - (length params))res

evStmt (Block vardef stmt1) dfx dpx st = let newState = addVarToState vardef st
                                             res = recBlock stmt1 dfx dpx newState
                                          in take (length res - (length vardef))res

recBlock :: [Stmt] -> [FunDef] -> [ProcDef] -> StateP -> StateP
recBlock (stm:stmt) dfx dpx st = recBlock stmt dfx dpx (evStmt stm dfx dpx st)
recBlock [] _ _ st = st

addVarToState :: [VarDef] -> StateP -> StateP
addVarToState (v:vardef) st = addVarToState vardef (updateValue i val st)
                              where (i,val) = initv v
addVarToState [] st = st

-- Задача 6 ------------------------------------
iswfExp :: Exp -> VarEnv -> FunEnv -> Maybe Type
iswfExp (Const _) _ _ = Just It 
iswfExp (Var v) ve _ = if containsKey v ve 
                           then Just (lookUp v ve)
                           else Nothing
iswfExp (OpApp op exp1 exp2) ve fe =
  let type1 = iswfExp exp1 ve fe
      type2 = iswfExp exp2 ve fe
  in case type1 of
    Just t1 -> case type2 of
               Just t2 -> iswfOp op [t1,t2]
               _       -> Nothing
    _  -> Nothing
                                    
iswfExp (Cond exp1 exp2 exp3) ve fe = 
  let type1 = iswfExp exp1 ve fe
      type2 = iswfExp exp2 ve fe
      type3 = iswfExp exp3 ve fe
  in case type1 of
    Just t1 -> case type2 of
                Just t2 -> case type3 of
                            Just t3 -> iswfCond [t1,t2,t3]
                            _       -> Nothing
                _      -> Nothing
    _ -> Nothing
                                
iswfExp (FunApp i exps) ve fe = if length exps == (length (lookUp i fe))
                                then if map (\e -> iswfExp e ve fe)exps == map(Just)(lookUp i fe)then Just It else Nothing
                                else Nothing
-- Задача 7 ------------------------------------
iswfStmt :: Stmt -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfStmt (Assign i exp1) ve fe _ = 
  case iswfExp exp1 ve fe of
    Just t -> case varEnvType i ve of
              Just vt -> t == vt
              Nothing -> False
    Nothing -> False

iswfStmt (AssignA i exp1 exp2) ve fe _ = 
  let typeExp1 = iswfExp exp1 ve fe
      typeExp2 = iswfExp exp2 ve fe
      typeI = varEnvType i ve
  in case typeExp1 of
      Just t1 -> case typeExp2 of
                 Just t2 -> case typeI of
                            Just i1 -> iswfAssignA[i1,t1,t2]
                            Nothing -> False
                 Nothing -> False
      Nothing -> False

iswfStmt (If exp1 stmt1 stmt2) ve fe pe = 
  case iswfExp exp1 ve fe of
    Just It -> iswfStmt stmt1 ve fe pe && iswfStmt stmt2 ve fe pe
    _ -> False

iswfStmt (While exp1 stmt1) ve fe pe = 
  case iswfExp exp1 ve fe of
    Just It -> iswfStmt stmt1 ve fe pe
    _       -> False

iswfStmt (Call i exps) ve fe pe =
  case functType i pe of
    Just t -> case expsType exps ve fe of
      Just ts -> ts == t
      Nothing -> False
    Nothing -> False
iswfStmt (Block vars stmt1) ve fe pe = let newVe = addVardefToVarEnv vars ve
                                       in and (map (\e -> iswfStmt e newVe fe pe) stmt1)

-- Задача 8 ------------------------------------
iswfFunDef :: FunDef -> FunEnv -> Bool
iswfFunDef (i,(vardefs,exp1)) fe =
 let ft = case functType i fe of
          Just t -> (map varDefType vardefs == t)
          Nothing -> False
     ve = map initVarType vardefs
     expType = case iswfExp exp1 ve fe of
               Just It -> True
               _  -> False
  in ft && expType

iswfProcDef :: ProcDef -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfProcDef (i,(vardefs, stmt1)) ve fe pe =
  let newVe = addVardefToVarEnv vardefs ve
      correctFunct = case functType i pe of
                     Just t -> (map varDefType vardefs)== t
                     Nothing -> False
   in correctFunct && (iswfStmt stmt1 newVe fe pe)

-- Задача 9 ------------------------------------
iswfProgram :: Program -> Bool
iswfProgram (vd, fd, pd)= 
  let ve = map initVarType vd
      fe = map initFunType fd
      pe = map initProcType pd
      correctFE = and (map (\e -> iswfFunDef e fe) fd)
      correctPE = and ( map (\e -> iswfProcDef e ve fe pe) pd)
      correctMain = elem ("main", [])pe
      numParam = (map fst ve) ++ (map fst fe) ++ (map fst pe)
      correctParams = length numParam == length (nub numParam)
   in correctFE && correctPE && correctMain && correctParams 



containsKey :: Eq a => a -> [(a,b)] -> Bool
containsKey a ((al,_):abc) = if a == al then True else containsKey a abc
containsKey _ [] = False

addVardefToVarEnv::[VarDef] -> VarEnv -> VarEnv
addVardefToVarEnv ((Arr i):vardefs) ve = (i,At):(addVardefToVarEnv vardefs ve)
addVardefToVarEnv ((Int i):vardefs) ve = (i,It):(addVardefToVarEnv vardefs ve)
addVardefToVarEnv [] ve = ve

varEnvType:: Id -> VarEnv -> Maybe Type
varEnvType i ve = if containsKey i ve then Just (lookUp i ve) else Nothing

varDefType :: VarDef -> Type
varDefType (Arr _) = At
varDefType (Int _) = It

initVarType :: VarDef -> (Id, Type)
initVarType (Int value) = (value, It)
initVarType (Arr value) = (value, At) 

initFunType :: FunDef -> (Id, [Type])
initFunType (i, (vardefs, _)) = (i, map varDefType vardefs)

initProcType :: ProcDef -> (Id, [Type])
initProcType (i, (vardefs, _)) = (i, map varDefType vardefs)

functType :: Id -> FunEnv -> Maybe [Type]
functType i fe = if containsKey i fe then Just (lookUp i fe) else Nothing

expsType:: [Exp] -> VarEnv -> FunEnv -> Maybe [Type]
expsType (ex:exps) ve fe = 
  case iswfExp ex ve fe of
    Just t -> case expsType exps ve fe of
      Just ts-> Just (t:ts)
      Nothing -> Nothing
    Nothing -> Nothing
expsType [] _ _ = Just []


correctStmtList :: [Stmt] -> VarEnv -> FunEnv -> ProcEnv -> Bool
correctStmtList (s:stmt) ve fe pe = iswfStmt s ve fe pe && (correctStmtList stmt ve fe pe)
correctStmtList [] _ _ _ = True

--- Допоміжні функції -----------------------------
lookUp :: Eq a => a -> [(a,b)] -> b
-- Передумова: Пара з ключом a є в списку пар abx
lookUp a abx = maybe (error "lookUp") id (lookup a abx) 

-- формує початкове значення змінної
initv :: VarDef -> (Id, Value)
initv (Arr v) = (v, A [])
initv (Int v) = (v, I 0) 

-- Реалізація виконання програми 
evProgram :: Program -> StateP 
evProgram (dvx, dfx, dpx) = 
   let sb = map initv dvx 
       ( _, s) = lookUp "main" dpx      
   in  evStmt s dfx dpx sb   

--  iswfOp o ts - перевіряє коректність типів операндів ts 
--     бінарної операції o і формує тип результату Just t або Nothing  
iswfOp :: Op -> [Type] -> Maybe Type 
iswfOp Add   [It,It] = Just It
iswfOp Minus [It,It] = Just It
iswfOp Mul   [It,It] = Just It
iswfOp Less  [It,It] = Just It
iswfOp Equal [It,It] = Just It
iswfOp Index [At,It] = Just It
iswfOp _      _      = Nothing

--  iswfCond ts - перевіряє коректність  типів операндів ts
--     умовного виразу і формує тип результату Just t або Nothing 
iswfCond :: [Type] -> Maybe Type 
iswfCond [It,It,It] = Just It
iswfCond [It,At,At] = Just At
iswfCond _          = Nothing 

-- iswfAssignA ts перевіряє коректність  типів операндів ts
--   операції присвоювання значення елементу масива 
iswfAssignA :: [Type] -> Bool
iswfAssignA [At,It,It] = True 
iswfAssignA _          = False  

---- Дані для тестування  -----------------------
-- Стан для тестування
sampleState :: StateP
sampleState = [("x",I 5),("y",I 2),("a", A [(2,3),(0,4), (1,2)])]

varEnv :: VarEnv 
varEnv = [("x",It), ("y",It), ("a",At)]

-- Функція максимум двох чисел 
-- func biggest(m,n)= (m<n ? n : m)
biggest :: FunDef
biggest =("biggest",
          ([Int "m", Int "n"], 
           Cond (OpApp  Less (Var "m") (Var "n"))  (Var "n")  (Var "m")                                                                
           )
         )
-- Функція, що обчислює число Фібоначчі
-- func fib(n) = (n<3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib = ("fib",
       ([Int "n"], 
        Cond (OpApp Less (Var "n") (Const 3))
             (Const 1)
             (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const 1)])
                        (FunApp "fib" [OpApp Minus (Var "n") (Const 2)]))
       )
      )

-- Функція - сума елементів масиву 0..n ...
-- func sumA(a[],n) = (n<0 ? 0 : a[n] + sumA (a,n-1))
sumA :: FunDef
sumA = ("sumA",
        ([Arr "a", Int "n"],
         Cond (OpApp Less (Var "n") (Const 0)) 
              (Const 0)
              (OpApp Add (OpApp Index (Var "a") (Var "n"))
                         (FunApp "sumA" [Var "a", OpApp Minus (Var "n")(Const 1)])
              )
        )
       )

funEnv :: FunEnv
funEnv = [("biggest",[It,It]),("fib", [It]),("sumA",[At,It])]

-- Приклад оператору - блоку 
sampleBlock :: Stmt 
sampleBlock = Block [Arr "b"]
                 [AssignA "b" (Const 0) (Const 9), AssignA "b" (Const 2) (Const 5),
                  AssignA "b" (Const 3) (Const 7), AssignA "b" (Const 5) (Const 1),
                  Call "sumA1" [Var "b", Const 5]
                 ]

-- Процедура - додавання двох чисел...
-- proc gAdd(x,y) gSum = x + y 
gAdd :: ProcDef
gAdd = ("gAdd", 
        ([Int "x", Int "y"], 
         Assign "gSum" (OpApp Add (Var "x") (Var "y"))
        )
       )

-- Процедура - сума елементів масиву 0..n ...
-- proc sumA1(a[],n) {i;limit;
--      sA=0; i=0; limit=n+1;
--      while (i<limit){sA=sA+a[i]; i=i+1}
--                   }
sumA1 :: ProcDef
sumA1 = ("sumA1",
         ([Arr "a", Int "n"], 
          Block [Int "i", Int "limit"] 
            [Assign "sA" (Const 0), Assign "i" (Const 0),
             Assign "limit" (OpApp Add (Var "n") (Const 1)),
             While (OpApp Less (Var "i") (Var "limit"))
                   (Block [] 
                     [Assign "sA" (OpApp Add (Var "sA")
                                  (OpApp Index (Var "a") (Var "i"))),
                      Assign "i" (OpApp Add (Var "i") (Const 1))
                     ]
                   )
            ]
         )
        )

procEnv :: ProcEnv 
procEnv = [("gAdd",[It,It]),("sumA1",[At,It])]

-- Повні програми
-- gSum; 
-- proc gAdd(x,y) gSum = x + y 
-- proc main() call gAdd(5,10)   
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main",([],Call "gAdd" [Const  5, Const 10]))])

-- sA
-- proc sumA1(a[],n) {i;limit; .... } 
-- proc main() {b[]; b[0]=9; b[2]=5; b[3]=7; b[5]=1;
--                   call sumA1 (b,5)
--             }

pr2 :: Program
pr2 = ([Int "sA"], [], 
       [sumA1, 
        ("main",([], sampleBlock))
       ])
