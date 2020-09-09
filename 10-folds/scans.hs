-- scanr :: (a -> b -> b) -> b -> [a] -> [b]
-- scanl :: (b -> a -> b) -> b -> [a] -> [b]

-- scanr (+) 0 [1..3]
-- f x q : qs where qs@(q:_) = scanr f q0 xs
-- (+) 1 (head (scanr (+) 0 [2..3])) : scanr (+) 0 [2..3]

-- scanr (+) 0 [2..3] =
-- (+) 2 (head (scanr (+) 0 [3])) : scanr (+) 0 [3]

-- scanr (+) 0 [3] =
-- (+) 3 (head (scanr (+) 0 [])) : scanr (+) 0 []

-- (scanr (+) 0 []) =
-- [0]

-- (+) 3 0 : [0]
-- (+) 2 ((+) 3 0) : ((+) 3 0 : [0])
-- (+) 1 ((+) 2 ((+) 3 0)) : ((+) 2 ((+) 3 0) : ((+) 3 0 : [0]))
-- 6 : (5 : 3 : [0])
-- [6, 5, 3, 0]

-- from an older spec
-- scanl :: (a -> b -> a) -> a -> [b] -> [a]
-- scanl f q ls =
--   q : (case ls of
--          []   -> []
--          x:xs -> scanl f (f q x) xs)

-- scanl (+) 0 [1..3]
-- 0 : scanl (+) ((+) 0 1) [2..3]
-- 0 : ((+) 0 1) : scanl (+) ((+) ((+) 0 1) 2) [3]
-- 0 : ((+) 0 1) : ((+) ((+) 0 1) 2) : scanl (+) ((+) ((+) ((+) 0 1) 2) 3) []
-- 0 : ((+) 0 1) : ((+) ((+) 0 1) 2) : ((+) ((+) ((+) 0 1) 2) 3) : []
-- 0 : 1 : 3 : 6 : []
-- [0, 1, 3, 6]


scanrAsFoldr :: (a -> b -> b) -> b -> [a] -> [b]

-- scanrAsFoldr f acc [] = [acc]
-- scanrAsFoldr f acc (x:xs) = foldr f acc (x:xs) : scanrAsFoldr f acc xs

scanrAsFoldr f z = foldr rf [z]
  where rf x (ac:acs) = f x ac : (ac:acs)

-- eval scanrAsFoldr (+) 0 [1..3]

-- foldr rf [0] [1..3] -- f x (foldr f z xs)
-- rf 1 (foldr rf [0] [2..3])

-- (foldr rf [0] [2..3]) =
-- rf 2 (foldr rf [0] [3])

-- (foldr rf [0] [3]) =
-- rf 3 (foldr rf [0] [])

-- foldr rf [0] [] = [0]

-- rf 3 [0] -- f x ac : (ac:acs)
-- (+) 3 0 : [0]
-- [3, 0]

-- rf 2 [3, 0]
-- (+) 2 3 : [3, 0]
-- [5. 3, 0]

-- rf 1 [5, 3, 0]
-- (+) 1 5 : [5, 3, 0]
-- [6, 5, 3, 0]
