{-# OPTIONS_GHC -Wall #-}
module Practice08 where
import Text.ParserCombinators.Parsec

num :: Parser Int
num = do digits <- many1 digit
         return $ read digits

infOp :: String -> (a->a->a) -> Parser (a->a->a)
infOp x f = do _ <- string x
               return f

mulop :: Parser (Int->Int->Int)
mulop = infOp "*" (*)

paren :: Parser a -> Parser a
paren p = do _ <- string "("
             v <- p
             _ <- string ")"
             return v

addop :: Parser (Int->Int->Int)
addop = infOp "+" (+) <|> infOp "-" (-)

factor :: Parser Int
factor = num <|> paren num

term, expr :: Parser Int
term = chainl1 factor mulop
expr = chainl1 factor addop

full ::Parser Int
full = do v <- expr
          eof
          return v 