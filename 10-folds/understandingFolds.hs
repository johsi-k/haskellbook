-- 1. foldr (*) 1 [1..5] will return the same result as
-- (b) foldl (flip (*)) 1 [1..5]
-- (c) foldl (*) 1 [1..5]

-- 2. Write out the evaluation steps for
-- foldl (flip (*)) 1 [1..3]
-- foldl (flip (*)) (flip (*) 1 1) [2..3]
-- foldl (flip (*)) (flip (*) (flip (*) 1 1) 2) [3]
-- foldl (flip (*)) (flip (*) (flip (*) (flip (*) 1 1) 2) 3) []
-- flip (*) (flip (*) (flip (*) 1 1) 2) 3
-- flip (*) (flip (*) 1 2) 3
-- flip (*) 2 3
-- 6

-- 3. One difference between foldr and foldl is
-- (c) foldr, but not foldl, associates to the right

-- 4. Folds are catamorphisms, which means they are generally used to
-- (a) reduce structure

-- 5. Fix the following folds
-- (a) foldr (++) ["woot" , "WOOT", "woot"]
-- foldr (++) [] ["woot" , "WOOT", "woot"]

-- (b) foldr max [] "fear is the little death"
-- max :: Ord a => a -> a -> a
-- foldr :: (Char -> Char -> Char) -> Char -> [Char] -> Char
-- foldr max 'a' "fear is the little death"

greatestChar :: String -> Maybe Char
greatestChar ""     = Nothing
greatestChar (x:xs) = Just (foldr max x xs)

-- foldr :: ([Char] -> [Char] -> [Char]) -> [Char] -> [[Char]] -> [Char]
-- foldr max [] ["fear" "is" "the" "little" "death"]

-- (c) foldr and True [False, True]
-- and :: [Bool] -> Bool
-- (&&) :: Bool -> Bool -> Bool
-- foldr (&&) True [False, True]

-- (d) foldr (||) True [False, True]
-- (||) :: Bool -> Bool -> Bool
-- foldr (||) False [False, True]

-- (e) foldl ((++) . show) "" [1..5]
-- ((++) . show) :: Show a => a -> [Char] -> [Char]
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- type signatures of reducing function do not match
-- foldr ((++) . show) "" [1..5]

-- (f) foldr const 'a' [1..5]
-- const :: a -> b -> a
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- type signatures of reducing function do not match
-- foldl const 'a' [1..5]

-- (g) foldr const 0 "tacos"
-- const :: a -> b -> a
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- type signatures of reducing function do not match
-- foldl const 0 "tacos"

-- (h) foldl (flip const) 0 "burritos"
-- (flip const) :: b -> c -> c
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- type signatures of reducing function do not match
-- foldr (flip const) 0 "burritos"

-- (i) foldl (flip const) 'z' [1..5]
-- (flip const) :: b -> c -> c
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- type signatures of reducing function do not match
-- foldr (flip const) 'z' [1..5]
