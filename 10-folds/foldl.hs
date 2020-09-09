-- note the different order of arguments in the reducing function
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b

-- foldl (+) 0 (1 : 2 : 3 : []) evaluates to
-- ((0 + 1) + 2) + 3

-- we can use scanr and scanl to see how folds evaluate
-- scanr (+) 0 [1..5] = [15,14,12,9,5,0]
-- scanl (+) 0 [1..5] = [0,1,3,6,10,15]

-- the result of folds can be obtained by
-- last (scanl f z xs) = foldl f z xs
-- head (scanr f z xs) = foldr f z xs

-- foldr (^) 2 [1..3] = 1
-- foldl (^) 2 [1..3] = 64

-- foldr (^) 2 [1..3] =
-- (^) 1 (foldlr (^) 2 [2..3]) -- f x (foldr f z xs)
-- (^) 1 ((^) 2 (foldr (^) 2 [3]))
-- (^) 1 ((^) 2 ((^) 3 (foldr (^) 2 [])))
-- (^) 1 ((^) 2 ((^) 3 2))
-- (^) 1 ((^) 2 (9))
-- (^) 1 512
-- 1

-- foldl (^) 2 [1..3] =
-- foldl (^) ((^) 2 1) [2..3] -- foldl f (f z x) xs
-- foldl (^) ((^) ((^) 2 1) 2) [3]
-- foldl (^) ((^) ((^) ((^) 2 1) 2) 3) []
-- (^) ((^) ((^) 2 1) 2) 3
-- (^) ((^) 2 2) 3
-- (^) 4 3
-- 64

-- diversion begin
-- why length (enumFromTo 1 3) is both strict in the spine and leaves
-- enumFromTo 1 3
--   = enumDeltaToInteger 1 1 3
--   = up_list 1 1 3
--   = 1 : go (1 + 1) -- length forces go to keep executing in order to evaluate its xs
--   = 1 : (2 : go (2 + 1))
--   = 1 : (2 : (3 : go (3 + 1)))
--   = 1 : (2 : (3 : []))

-- enumFromTo x lim = enumDeltaToInteger x 1 lim

-- enumDeltaToInteger x delta lim
--   | delta >= 0 = up_list x delta lim
--   | otherwise  = dn_list x delta lim

-- up_list x0 delta lim = go (x0 :: Integer)
--                     where
--                         go x | x > lim   = []
--                              | otherwise = x : go (x+delta)
-- diversion end

-- associativity affects argument order

-- e.g. folding with identity
-- foldr f z [1, 2, 3]
-- f ~ (:); z ~ []

-- foldr (:) [] [1, 2, 3]
-- (:) 1 (foldr (:) [] [2, 3])
-- (:) 1 ((:) 2 (foldr (:) [] [3]))
-- (:) 1 ((:) 2 ((:) 3 (foldr (:) [] [])))
-- (:) 1 ((:) 2 ((:) 3 []))
-- (:) 1 ((:) 2 (3 : []))
-- 1 : (2 : (3 : []))

-- foldl (:) [] [1, 2, 3]
-- foldl f (f z x) xs
-- foldl (:) ((:) [] 1) [2, 3] -- but (:) expects a value instead of a list as its first arg!
-- foldl (:) ((:) ((:) [] 1) 2) [3]
-- foldl (:) ((:) ((:) ((:) [] 1) 2) 3) []
-- (:) ((:) ((:) [] 1) 2) 3
-- (([] : 1) : 2) : 3

-- solution: we can flip each set of arguments around
-- fcons = flip (:)
-- (([] `fcons` 1) `fcons` 2) `fcons` 3
-- ([1] `fcons` 2) `fcons` 3
-- [2, 1] `fcons` 3
-- [3, 2, 1]
-- although we end up with a different result

-- const takes two args and always returns the first
-- foldr const 0 [1..5]
-- f x (foldr f z xs)
-- const 1 (foldr const 0 [2..5])
-- const doesn't evaluate its second arg so the rest of the fold is never evaluated

-- foldr (flip const) 0 [1..5]
-- (flip const) 1 (foldr (flip const) 0 [2..5])
-- foldr (flip const) 0 [2..5]
-- (flip const) 2 (foldr (flip const) 0 [3..5])
-- foldr (flip const) 0 [3..5]
-- (flip const) 3 (foldr (flip const) 0 [4..5])
-- foldr (flip const) 0 [4..5]
-- (flip const) 4 (foldr (flip const) 0 [5])
-- foldr (flip const) 0 [5]
-- (flip const) 5 (foldr (flip const) 0 [])
-- foldr (flip const) 0 []
-- 0

-- foldl (flip const) 0 [1..5]
-- foldl (flip const) (flip const 0 1) [2..5]
-- foldl (flip const) (flip const (flip const 0 1) 2) [3..5]
-- foldl (flip const) (flip const (flip const (flip const 0 1) 2) 3) [4..5]
-- foldl (flip const) (flip const (flip const (flip const (flip const 0 1) 2) 3) 4) [5]
-- foldl (flip const) (flip const (flip const (flip const (flip const (flip const 0 1) 2) 3) 4) 5) []
-- (flip const (flip const (flip const (flip const (flip const 0 1) 2) 3) 4) 5)
-- 5
