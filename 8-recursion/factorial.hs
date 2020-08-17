module Factorial where

factorial :: Integer -> Integer
factorial 0 = 1 -- base case
factorial n = n * factorial (n - 1)

-- factorial 4 =
--   4 * factorial (4 - 1)
--   4 * factorial 3

--   -- evaluate factorial applied to 3 (ln 5)
--   4 * 3 * factorial (3 - 1)
--   4 * 3 * factorial 2

--   -- evaluate factorial applied to 2 (ln 5)
--   4 * 3 * 2 * factorial (2 - 1)
--   4 * 3 * 2 * factorial 1

--   -- evaluate factorial applied to 1 (ln 5)
--   4 * 3 * 2 * 1 * factorial (1 - 1)
--   4 * 3 * 2 * 1 * factorial 0

--   -- evaluate factorial applied to 0 (base case)
--   4 * 3 * 2 * 1 * 1

--   -- evaluate multiplications
--   24
