{-# OPTIONS_GHC -Wall #-}
module Practice04 where

import Data.Char

c,b :: String -> Maybe String
c('d':st1) = case c st1 of
    Just ('d':st2) -> Just st2
    _              -> Nothing
c('a':st1) = case b st1 of
    Just ('a':st2) -> Just st2
    _              -> Nothing
c _ = Nothing

b('e':st1)= b st1
b st1     = Just st1

analyseG :: String -> Bool
analyseG st1 = case c st1 of
    Just st2 -> null st2
    Nothing -> False

analyseB :: String -> Bool
analyseB st1 = case s st1 of 
    Just st2 -> null st2
    Nothing -> False

--maybe :: b -> (a -> b) -> Maybe a -> b
-- analyseB st1 = maybe False null (s st1)
-- analyseB = (maybe False null) s 


s:: String -> Maybe String
s ('(':st1) = case s st1 of
    Just (')':st2) -> s st2
    _              -> Nothing
s st = Just st

sC :: String -> Maybe (Int, String)
sC ('(':st1) = case sC st1 of 
    Just (v1,')':st2) -> case sC st2 of
        Just (v2, st3) -> Just (v1+v2+1, st3)
        Nothing        -> Nothing
    _ -> Nothing
sC st = Just (0,st)

evalC :: String -> Maybe Int
evalC st1 = case sC st1 of
    Just (v, st2) | null st2 -> Just v
    _                        -> Nothing

sD :: String -> Maybe (Int, String)
sD ('(':st1) = case sD st1 of
    Just (v1,')':st2) -> case sD st2 of
        Just(v2, st3)    -> Just (if v1<v2 then v2 else v1+1,st3)
        Nothing          -> Nothing
    _                 -> Nothing
sD st        = Just (0,st)

evalD :: String -> Maybe Int
evalD st1 = case sD st1 of
    Just (v,st2) | null st2 -> Just v
    _           -> Nothing


e,a,t :: String -> Maybe String

e st1 = case t st1 of
    Just st2 -> a st2
    Nothing -> Nothing

a (p:st1) | elem p "+*" = case t st1 of
                    Just st2 -> a st2
                    Nothing  -> Nothing
a st1 = Just st1

t ('(':st1) = case e st1 of
    Just (')':st2) -> Just st2
    _              -> Nothing
t (d:st1) | elem d "12" = Just st1
t _ = Nothing

analyseE :: String -> Bool
analyseE st1 = case e st1 of
    Just st2 -> null st2
    _        -> False


eE, tE :: String -> Maybe (Int, String)
eE st1 = case tE st1 of
    Just (v1,st2) -> aE (v1, st2)
    Nothing       -> Nothing

tE ('(':st1) = case eE st1 of
    Just (v,(')':st2)) -> Just(v,st2)
    _                  -> Nothing

tE (d:st1) | elem d "12" = Just (digitToInt d, st1)
tE _ = Nothing

aE :: (Int, String) -> Maybe (Int, String)
aE (v1,(d:st1))| elem d "+*" = case tE st1 of  
     Just (v2,st2) -> let v = if d=='+' then v1+v2 else v1*v2  
                      in aE (v,st2)  
     Nothing       -> Nothing 
aE (v1,st1)                  = Just (v1,st1)

evalE :: String -> Maybe Int
evalE st1 = case eE st1 of 
   Just (v,st2)| null st2 -> Just v 
   _                      -> Nothing 

