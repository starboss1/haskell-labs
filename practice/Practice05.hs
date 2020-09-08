{-# OPTIONS_GHC -Wall #-}
module Practice05 where

putN :: Int -> String -> IO()
putN n str = if n <= 1 then putStrLn str 
    else do putStrLn str 
            putN (n-1) str

takeN :: String -> String -> String
takeN file ns = unlines $ take (read ns) (lines file)

firstN :: IO()
firstN = do putStr "file>"
            x <- getLine 
            putStr "N>"
            y <- getLine
            str <- readFile x
            putStr $ takeN str y
    

takeLN :: String -> String -> String
takeLN file ns = (unlines . reverse .((take.read)ns).reverse .lines)file

lastN :: IO()
lastN = do putStr "file>"
           x <- getLine 
           putStr "N>"
           y <- getLine
           str <- readFile x
           putStr $ takeLN str y