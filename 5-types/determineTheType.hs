{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

-- simple example
-- example = 1

-- 1.
-- a)
-- (* 9) 6
-- 54 :: Num a => a

-- b)
-- head [(0, "doge"), (1, "kitteh")]
-- (0, "doge") :: Num a => (a, [Char])

-- c)
-- head [(0 :: Integer, "doge"), (1, "kitteh")]
-- (0, "doge") :: (Integer, [Char])

-- d)
-- if False then True else False
-- False :: Bool

-- e)
-- length [1, 2, 3, 4, 5]
-- 5 :: Int

-- f)
-- (length [1, 2, 3, 4]) > (length "TACOCAT")
-- False :: Bool

-- 2.
x = 5
y = x + 5
w = y * 10
w :: Num a => a

-- 3.
z y = y * 10
z :: Num a => a -> a

-- 4.
f = 4 / y
f :: Fractional a => a

-- 5.
x' = "Julie"
y' = " <3 "
z' = "Haskell"
f' = x' ++ y' ++ z'
f' :: [Char]
