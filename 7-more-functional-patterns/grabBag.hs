-- 1. Which (two or mare) of the following are equivalent?
-- a) mTh x y z = x * y * z
-- b) mTh x y z = \z -> x * y * z
-- c) mTh x = \y -> \z -> x * y * z
-- d) mTh = \x -> \y -> \z -> x * y * z
-- a, c and d are equivalent

-- 2.
-- The type of mTh is Num a => a -> a -> a -> a.
-- Which is the type of mTh 3?
-- d) Num a => a -> a -> a

-- 3
-- a) Rewrite the f function in the where clause.
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  -- where f n = n + 1
  where f = \m -> m + 1

-- b) Rewrite the following to use anonymous lambda syntax
-- addFive x y = (if x > y then y else x) + 5
addFive = \x y -> (if x > y then y else x) + 5

-- c) Rewrite the following so it doesn't use anonymous lambda syntax
-- mflip f = \x -> \y -> f y x
mflip f x y = f y x
