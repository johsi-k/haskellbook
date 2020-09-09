-- Warm-up and Review
-- 1. Given the following sets of consonants and vowels
-- stops = "pbtdkg"
-- vowels = "aeiou"

-- (a) Write a function that takes inputs from stops and vowels and makes 3-tuples of all possible stop-vowel-stop combinations. These will not all correspond to real words in English, although the stop-vowel-stop pattern is common enough that many of them will.
-- makeWords :: String -> String -> [(Char, Char, Char)]
-- makeWords :: [a] -> [a] -> [(a, a, a)]
makeWords :: [a] -> [b] -> [(a, b, a)]
makeWords stops vowels = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

-- (b) Modify the function so that it only returns combinations beginning with p.
makePWords :: String -> String -> [(Char, Char, Char)]
-- makePWords stops vowels = [(x, y, z) | x <- stops, x == 'p', y <- vowels, z <- stops]
makePWords stops vowels = [('p', y, z) | y <- vowels, z <- stops]

-- (c) Now set up lists of nouns and verbs (instead of stops and vowels) and modify the function to make tuples representing possible noun-verb-noun sentences.
-- makeSentences :: [String] -> [String] -> [(String, String, String)]
makeSentences :: [a] -> [b] -> [(a, b, a)]
makeSentences nouns verbs = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]

-- 2. What does the following mystery function do?
seekritFunc :: String -> Int
seekritFunc x =
    div (sum (map length (words x)))
        (length (words x))
-- it returns the average length of each word in the given sentence

-- 3. Can you rewrite that using fractional division?
seekritFunc' :: String -> Double
-- seekritFunc' x = fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))
seekritFunc' x = (fromIntegral . sum . map length . words $ x) / (fromIntegral . length . words $ x)

-- Rewriting functions using folds
-- foldr :: (a -> b -> b) -> b -> [a] -> b

-- 1.
-- myOr returns true if any Bool in the list is True
myOr :: [Bool] -> Bool
-- myOr = foldr (\a b -> if a then True else b) False
myOr = foldr (||) False


-- 2.
-- myAny returns True if a -> Bool applied to any of the values in the list returns True
myAny :: (a -> Bool) -> [a] -> Bool
-- myAny f = foldr (\a b -> if f a then True else b) False
myAny f = foldr (\a b -> f a || b) False

myAnyOr f = myOr . map f


-- 3. Write two versions of myElem. One version should use folding and the other should use any.
myElem :: Eq a => a -> [a] -> Bool
-- myElem e = foldr (\a b -> if e == a then True else b) False
myElem e = foldr (\a b -> (e == a) || b) False

-- any :: (a -> Bool) -> [a] -> Bool
myElemAny e = any (== e)


-- 4. Implement myReverse, don't worry about making it lazy
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []


-- 5. Write myMap in terms of foldr. It should have the same behaviour as map.
myMap :: (a -> b) -> [a] -> [b]
-- myMap f = foldr (\x acc -> f x : acc) []
-- myMap f = foldr (\x -> (f x :)) [] -- sectioning
myMap f = foldr ((:) . f) [] -- point free


-- 6. Rewrite myFilter in terms of foldr. It should have the same behaviour as filter.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x : acc else acc) []


-- 7. squish flattens a list of lists into a list
squish :: [[a]] -> [a]
-- squish = foldr (\x acc -> x ++ acc) []
squish = foldr (++) []


-- 8. squishMap maps a function over a list and concatenates the results
squishMap :: (a -> [b]) -> [a] -> [b]
-- squishMap f xs = squish (map f xs)
-- squishMap f = squish . map f

-- squishMap f = foldr (\x -> (f x ++)) []
-- squishMap f = foldr (\x -> (++) (f x)) []
squishMap f = foldr ((++) . f) []


-- 9. squishAgain flattens a list of lists into a list. Reuse the squishMap function.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id


-- 10.
-- myMaximumBy takes a comparison function and a list and returns the greatest element of the list based on the last value that the comparison returned GT for.
-- myMaximumBy :: (a -> a -> Ordering)
--             -> [a]
--             -> a
-- myMaximumBy f (x:xs) = foldr (\ele acc -> if f ele acc == GT then ele else acc) x xs
-- myMaximumBy cmp (x:xs) = foldr rf x xs
--   where rf ele acc | x `cmp` acc == GT = ele
--                    | otherwise         = acc

myMaximumBy :: (a -> a -> Ordering)
            -> [a]
            -> Maybe a
myMaximumBy cmp [] = Nothing
myMaximumBy cmp (x:xs) = Just (foldr rf x xs)
  where rf ele acc | x `cmp` acc == GT = ele
                   | otherwise         = acc


-- 11. myMinimumBy takes a comparison function and a list and returns the least element of the list based on the last value that the comparison returned LT for.
-- myMinimumBy :: (a -> a -> Ordering)
--             -> [a]
--             -> a
myMinimumBy :: (a -> a -> Ordering)
            -> [a]
            -> Maybe a
myMinimumBy cmp [] = Nothing
myMinimumBy cmp (x:xs) = Just (foldr rf x xs)
  where rf ele acc | x `cmp` acc == LT = ele
                   | otherwise         = acc

