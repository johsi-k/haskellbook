module WordNumber where

-- import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = undefined

digits :: Int -> [Int]
-- with tail recursion
-- digits n = go n acc
  -- where
  --   go num acc
  --     | div num 10 == 0 = num : acc
  --     | otherwise = go (div num 10) (mod num 10 : acc)

-- with simple recursion
digits n
  | div n 10 == 0 = [n]
  | otherwise = digits (div n 10) ++ digits (mod n 10)

-- wordNumber :: Int -> String
-- wordNumber n = intercalate "-" $ map digitToWord (digits n)

-- or recursively
wn :: [Int] -> String
wn [] = "meh"
wn [x] = digitToWord x
wn (x:xs) = wn [x] ++ "-" ++ wn xs

wordNumber :: Int -> String
wordNumber = wn . digits
