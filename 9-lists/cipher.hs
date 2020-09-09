module Cipher where

import Data.Char

-- Write a basic Caesar cipher that shifts rightward
-- You can start by having the number of spaces to shift fixed, but it's more challenging to write a cipher that allows you to vary the number of shifts

-- these functions may be used for shifting
-- :t chr
-- chr :: Int -> Char

-- :t ord
-- ord :: Char -> Int

-- the shift should wrap back around to the beginning of the alphabet
-- rightward shift of 3 from 'z' should result in 'c'

alphaNum :: [(Char, Int)]
alphaNum = zip ['a'..'z'] [1..26]

charToNum :: Char -> Int
-- charToNum char = snd . head $ filter (\x -> fst x == char) alphaNum
-- charToNum char = snd . head $ filter ((== char) . fst) alphaNum
charToNum c = ord c - ord 'a'

numToChar :: Int -> Char
-- numToChar int = fst . head $ filter ((== int) . snd) alphaNum
numToChar n = chr (n + ord 'a')

caesar :: Int -> Char -> Char
-- caesar offset char = numToChar (mod (charToNum char + offset) 26)
-- caesar offset char = numToChar ((`mod` 26) (charToNum char + offset))
-- caesar offset char = numToChar $ (`mod` 26) (charToNum char + offset)
caesar offset char = numToChar $ (`mod` 26) $ charToNum char + offset

uncaesar :: Int -> Char -> Char
uncaesar offset char = numToChar $ (`mod` 26) $ charToNum char - offset
