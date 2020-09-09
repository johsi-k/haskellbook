-- filter :: (a -> Bool) -> [a] -> [a]
-- filter _ [] = []
-- filter pred (x:xs)
--   | pred x    = x : filter pred xs
--   | otherwise = filter pred xs

-- filter can handle many types of args
-- filter (\x -> (rem x 2) == 0) [1..20]
-- > [2,4,6,8,10,12,14,16,18,20]

-- list comprehensions as a means to filter
-- filter (\x -> elem x "aeiou") "abracadabra"
-- > "aaaaa"

-- [x | x <- "abracadabra", elem x "aeiou"]
-- > "aaaaa"

-- Exercises
-- 1. Write a filter function that gives all multiples of 3 out of a list from 1-30
mult3 :: [Integer]
mult3 = filter (\x -> rem x 3 == 0) [1..30]

-- 2. Compose the above function with the length function to tell us how many multiples of 3 there are between 1 and 30
countMult3 :: Int
countMult3 = length . filter ((== 0) . (`rem` 3)) $ [1..30]

-- 3. Remove all articles from a given sentence. You may reuse the earlier function that separates a string into a list of strings by separating them at spaces.
splitString :: Char -> String -> [String]
splitString char "" = []
splitString char (x:xs)
  | char == x = splitString char xs
  | otherwise = takeWhile (/= char) (x:xs) : splitString char (dropWhile (/= char) (x:xs))

myFilter :: String -> [String]
myFilter = filter (`notElem` ["the", "a", "an"]) . splitString ' '
