-- 1.
k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

-- a) What is the type of k?
-- k :: (a, b) -> a

-- b) What is the type of k2? Is it the same type as k1 or k3?
-- k2 :: [Char]

-- c) Of k1, k2, k3, which will return the number 3 as the result?
-- k3

-- 2. Fill in the definiton of the following function:
f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
f (a, _, c) (d, _, eff) = ((a, d), (c, eff))
