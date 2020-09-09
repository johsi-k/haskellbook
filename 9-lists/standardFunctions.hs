-- 1. myOr returns True if any Bool in the list is True
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs)
  | x         = True
  | otherwise = myOr xs


-- 2. myAny returns True if a -> Bool applied to any of the values in the list returns True
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)
  | f x       = True
  | otherwise = myAny f xs


-- 3. After you write the recursive myElem, write another version that uses any
myElem :: Eq a => a -> [a] -> Bool
myElem e [] = False
myElem e (x:xs)
  | e == x    = True
  | otherwise = myElem e xs

myElemAny :: Eq a => a -> [a] -> Bool
-- myElemAny elem list = any (== elem) list
-- myElemAny e = any (== e)
-- myElemAny e = any . (==) $ e
myElemAny = any . (==)


-- 4. Implement myReverse
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]


-- 5. squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs -- x is a list and xs a list of lists


-- 6. squishMap maps a function over a list and concats the results
squishMap :: (a -> [b]) -> [a] -> [b]
-- squishMap f xs = squish (map f xs)
-- squishMap f xs = squish . map f $ xs
-- squishMap f = squish . map f
-- squishMap f = (squish .) (map f)
-- squishMap = \f -> (squish .) (map f)
-- squishMap = \f -> (squish .) . map $ f
squishMap = (squish .) . map


-- 7. squishAgain flattens a list of lists into a list. This time reuse squishMap
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id


-- 8. myMaximumBy takes a comparison function and a list and returns the greatest element of the list based on the last value that the comparison returned GT for
-- maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
myMaximumBy :: (a -> a -> Ordering)
            -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy f (x1:x2:xs)
  | f x1 x2 == GT = myMaximumBy f (x1:xs)
  | otherwise     = myMaximumBy f (x2:xs)

-- handle empty lists
myMaximumBy' :: (a -> a -> Ordering)
             -> [a] -> Maybe a
myMaximumBy' _ []  = Nothing
myMaximumBy' _ [x] = Just x
myMaximumBy' f (x1:x2:xs)
  | f x1 x2 == GT = myMaximumBy' f (x1:xs)
  | otherwise     = myMaximumBy' f (x2:xs)


-- 9. myMinimumBy takes a comparison function and a list and returns the least element of the list based on the last value that the comparison returned LT for
myMinimumBy :: (a -> a -> Ordering)
            -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy f (x1:x2:xs)
  | f x1 x2 == LT = myMinimumBy f (x1:xs)
  | otherwise     = myMinimumBy f (x2:xs)

-- handle empty lists
myMinimumBy' :: (a -> a -> Ordering)
            -> [a] -> Maybe a
myMinimumBy' _ []  = Nothing
myMinimumBy' _ [x] = Just x
myMinimumBy' f (x1:x2:xs)
  | f x1 x2 == LT = myMinimumBy' f (x1:xs)
  | otherwise     = myMinimumBy' f (x2:xs)


-- 10. Using myMinimumBy and myMaximumBy, write your own versions of maximum and minimum
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
