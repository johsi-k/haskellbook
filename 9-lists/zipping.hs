-- :t zip
-- zip :: [a] -> [b] -> [(a, b)]

-- zip [1, 2, 3] [4, 5, 6]
-- > [(1,4),(2,5),(3,6)]

-- zip proceeds until the shortest list ends
-- zip [1, 2] [4, 5, 6]
-- > [(1,4),(2,5)]

-- zip [] [1..100000]
-- > []

-- unzip recovers lists as they were before they were zipped
-- unzip $ zip [1, 2, 3] [4, 5, 6]
-- > ([1,2,3],[4,5,6])

-- fst $ unzip $ zip [1, 2, 3] [4, 5, 6]
-- > [1,2,3]

-- snd $ unzip $ zip [1, 2, 3] [4, 5, 6]
-- > [4,5,6]

-- information can be lost if the lists are of unequal length
-- snd $ unzip $ zip [1, 2] [4, 5, 6]
-- > [4,5]


-- :t zipWith
-- > zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- the type variables of the args and result align with the type variables in the lists

-- zipWith (+) [1, 2, 3] [10, 11, 12]
-- > [11,13,15]

-- zipWith (*) [1, 2, 3] [10, 11, 12]
-- > [10,22,36]

-- zipWith (==) ['a'..'f'] ['a'..'m']
-- > [True,True,True,True,True,True]


-- Exercises
-- 1. Write your own version of zip
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _          = []
myZip _ []          = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

-- 2. Do the same for zipWith
myZipWith :: (a -> b -> c)
          -> [a] -> [b] -> [c]
myZipWith _ [] _          = []
myZipWith _ _ []          = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

-- 3. Rewrite your zip in terms of your zipWith
myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (,)
