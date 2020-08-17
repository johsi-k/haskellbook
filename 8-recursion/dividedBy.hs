-- Division with recursive subtraction

-- since we want to return quotient and remainder,
-- we need a tuple as the result of our function
-- dividedBy :: Integral a => a -> a -> (a, a)
-- original:
-- dividedBy num denom = go num denom 0
--   where go n   d count -- go allows a third arg count to track no. of subtractions
--          | n < d = (count, n)
--          | otherwise =
--              go (n - d) d (count + 1)

-- not quite there, doesn't work when rem is 0
-- should also decouple sign-fixing from division logic
-- dividedBy num denom = go num denom 0
--   where go n   d count =
--           case (signum n, signum d) of
--             (1, 1)   | n < d -> (count, n)
--                      | otherwise -> go (n - d) d (count + 1)
--             (-1, -1) | n < d -> (count, -n)
--                      | otherwise -> go (n - d) d (count + 1)
--             (1, -1)  | n < abs d -> (-count, n)
--                      | otherwise -> go (n - abs d) d (count + 1)
--             (-1, 1)  | abs n < d -> (-count, -n)
--                      | otherwise -> go (abs n - d) d (count + 1)

data DividedResult a =
    Result (a, a)
  | DividedByZero
  deriving Show
dividedBy :: Integral a => a -> a -> DividedResult a
dividedBy _ 0 = DividedByZero
dividedBy num denom = fixSign $ go (abs num) (abs denom) 0
  where
    fixSign (q, r) =
      case (signum num, signum denom) of
        (1, 1)   -> Result (q, r)
        (-1, 1)  -> Result (-q, -r)
        (1, -1)  -> Result (-q, r)
        (-1, -1) -> Result (q, -r)
        (0, _)   -> Result (0, 0)
        _        -> undefined
    go n d count
      | n < d = (count, n)
      | otherwise =
          go (n - d) d (count + 1)

-- with laziness
-- dividedBy :: Integral a => a -> a -> DividedResult a
-- dividedBy num denom =
--   case (signum num, signum denom) of
--     (1, 1)   -> Result (q, r)
--     (-1, 1)  -> Result (-q, -r)
--     (1, -1)  -> Result (-q, r)
--     (-1, -1) -> Result (q, -r)
--     (0, _)   -> Result (0, 0)
--     (_, 0)   -> DividedByZero
--     _        -> undefined
--     where
--       (q, r) = go (abs num) (abs denom) 0
--       go n d count
--         | n < d = (count, n)
--         | otherwise =
--             go (n - d) d (count + 1)


-- Evals
-- old way without count
-- dividedBy 10 2 ==
--   10 - 2, 8 (subtracted once)
--      - 2, 6 (subtracted twice)
--      - 2, 4 (subtracted thrice)
--      - 2, 2 (subtracted 4 times)
--      - 2, 0 (subtracted 5 times)

-- with count
-- dividedBy 10 2 =
--   go 10 2 0

--   | 10 < 2 = ...
--   -- false, skip this

--   | otherwise = go (10 - 2) 2 (0 + 1)

--   | go 8 2 1
--   -- 8 is not < 2, so otherwise branch

--   | go (8 - 2) 2 (1 + 1)
--   | go 6 2 2
--   -- 6 is not < 2, so otherwise branch

--   | go (6 - 2) 2 (2 + 1)
--   | go 4 2 3
--   -- 4 is not < 2, so otherwise branch

--   | go (4 - 2) 2 (3 + 1)
--   | go 2 2 4
--   -- 2 is not < 2, so otherwise branch

--   | go (2 - 2) 2 (4 + 1)
--   | go 0 2 5
--   -- 0 < 2, so n < d branch

--   | 0 < 2 = (5, 0)

--   (5, 0)
