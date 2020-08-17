module Reverse where

addExclamation :: String -> String
addExclamation string = string ++ "!"

extractFifthChar :: String -> Char
extractFifthChar string = string !! 4

dropFirstNine :: String -> String
dropFirstNine string = drop 9 string

thirdLetter :: String -> Char
thirdLetter x = x !! 2 

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome" !! (x - 1)

rvrs :: String -> String
rvrs string = last ++ middle ++ first
  where firstMiddle = take 9 string
        first = take 5 firstMiddle
        middle = drop 5 firstMiddle
        last = drop 9 string

main :: IO ()
main = print (rvrs "curry is awesome")