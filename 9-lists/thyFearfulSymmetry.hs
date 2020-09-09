
-- take :: Int -> [a] -> [a]
-- drop :: Int -> [a] -> [a]
-- splitAt :: Int -> [a] -> ([a], [a])

-- stops taking as soon as condition is not met
-- takeWhile :: (a -> Bool) -> [a] -> [a]

-- stops dropping as soon as condition is not met
-- dropWhile :: (a -> Bool) -> [a] -> [a]

-- 1. Using takeWhlie and dropWhile, write a function that
-- takes a string and returns a list of strings
-- using spaces to separate the elements of the string into words

-- removeLeadSpace :: String -> String
-- removeLeadSpace = dropWhile (== ' ')

-- removeLeadWord :: String -> String
-- removeLeadWord = dropWhile (/= ' ')

-- extractLeadWord :: String -> String
-- extractLeadWord = takeWhile (/= ' ')

-- myWords :: String -> [String]
-- myWords str
--   | removeLeadWord str == "" = [str]
--   | otherwise = extractLeadWord str : myWords (removeLeadSpace . removeLeadWord $ str)

myWords :: String -> [String]
myWords "" = []
myWords (' ':xs) = myWords xs
myWords xs = takeWhile (/= ' ') xs : myWords (dropWhile (/= ' ') xs)

-- 2. Write a function that takes a string and returns a list of strings
-- using newline separators to break up the string
-- see poemLines.hs

-- 3.
splitString :: Char -> String -> [String]
-- splitString char str
--   | removeLeadChar str == "" = [str]
--   | otherwise = extractLeadWord str : splitString char (removeLeadChar . removeLeadWord $ str)
--   where removeLeadChar = dropWhile (== char)
--         removeLeadWord = dropWhile (/= char)
--         extractLeadWord = takeWhile (/= char)

splitString char "" =  []
splitString char (x:xs)
  | char == x = splitString char xs
  | otherwise = takeWhile (/= char) (x:xs) : splitString char (dropWhile (/= char) (x:xs))

myWords' :: String -> [String]
myWords' = splitString ' '

myLines' :: String -> [String]
myLines' = splitString '\n'
