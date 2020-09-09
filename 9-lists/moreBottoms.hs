import Data.Bool

-- Will the following expressions return a value or be ⊥?
-- 1. take 1 $ map (+1) [undefined, 2, 3]
-- blows up

-- 2. take 1 $ map (+1) [1, undefined, 3]
-- returns [2]

-- 3. take 2 $ map (+1) [1, undefined, 3]
-- blows up

-- 4. What does the mystery function do? What is its type?
-- the mystery function checks for vowels in a given string
-- returning a list of Bools indicating True if a vowel is present and False otherwise
itIsMystery :: String -> [Bool]
-- itIsMystery xs = map (\x -> elem x "aeiou") xs
itIsMystery = map (\x -> elem x "aeiou")

-- 5. What will be the result of the following functions:
-- a) map (^2) [1..10]
-- [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

-- b) map minimum [[1..10], [10..20], [20..30]]
-- [1, 10, 20]

-- c) map sum [[1..5], [1..5], [1..5]]
-- [15, 15, 15]

-- Write a function that behaves the same way as the map (if-then-else) function
-- but uses bool from Data.Bool instead of if-then-else
-- bool :: a -> a -> Bool -> a
-- bool x y p evaluates to x when p is False, and evaluates to y when p is True
mapBool :: [Integer]
mapBool = map (\x -> bool x (-x) (x == 3)) [1..10]
