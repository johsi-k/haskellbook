inc :: Num a => a -> a
inc = (+1)

three = inc . inc . inc $ 0
three' = (inc . inc . inc) 0

-- a general function that can apply inc an indefinite number of times
-- times is a variable representing the number of times
-- the incrementing function (1 +) should be applied to the arg n
incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + incTimes (times - 1) n

-- e.g.
-- incTimes 3 0 =
--   1 + (incTimes (3 - 1) 0)
--   1 + (incTimes 2 0)

--   1 + 1 + (incTimes (2 - 1) 0)
--   1 + 1 + (incTimes 1 0)

--   1 + 1 + 1 + (incTimes (1 - 1) 0)
--   1 + 1 + 1 + (incTimes 0 0)

--   1 + 1 + 1 + (incTimes 0 0)
--   1 + 1 + 1 + 0
--   3

applyTimes :: (Eq a, Num a) =>
               a -> (b -> b) -> b -> b
-- applyTimes 0 f b = 0
-- applyTimes n f b = f . applyTimes (n-1) f $ b
applyTimes 0 = const id
applyTimes n = \f -> f . applyTimes (n-1) f

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times = applyTimes times (+1)

-- applyTimes 5 (+1) 5 =
--   (+1) . applyTimes (5-1) (+1) $ 5
--   (+1) . applyTimes 4 (+1) $ 5

--   (+1) . (+1) . applyTimes (4-1) (+1) $ 5
--   (+1) . (+1) . applyTimes 3 (+1) $ 5

--   (+1) . (+1) . (+1) . applyTimes (3-1) (+1) $ 5
--   (+1) . (+1) . (+1) . applyTimes 2 (+1) $ 5

--   (+1) . (+1) . (+1) . (+1) . applyTimes (2-1) (+1) $ 5
--   (+1) . (+1) . (+1) . (+1) . applyTimes 1 (+1) $ 5

--   (+1) . (+1) . (+1) . (+1) . (+1) . applyTimes (1-1) (+1) $ 5
--   (+1) . (+1) . (+1) . (+1) . (+1). applyTimes 0 (+1) $ 5
--   (+1) . (+1) . (+1) . (+1) . (+1) . 5

--   10

