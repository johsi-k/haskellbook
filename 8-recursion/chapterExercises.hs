-- Review of types
-- 1. What is the type of [[True, False], [True, True], [False, True]]?
-- d) [[Bool]]

-- 2. Which of the following has the same type as
-- [[True, False], [True, True], [False, True]]?
-- b) [[3 == 3], [6 > 5], [3 < 4]]

-- 3. For the following function
func :: [a] -> [a] -> [a]
func x y = x ++ y
-- which is true?
-- d) all of the obove

-- 4. For the func code above, which is a valid application of func to both its args?
-- b) "Hello" "World"


-- Reviewing currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- fill in the types
flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- 1. What is the value of appedCatty "woohoo!" ?
-- "woops mrow woohoo!"

-- 2. frappe "1" = "1 mrow haha"

-- 3. frappe (appedCatty "2") = "woops mrow 2 mrow haha"

-- 4. appedCatty (frappe "blue") = "woops mrow blue mrow haha"

-- 5. cattyConny (frappe "pink")
--               (cattyConny "green" (appedCatty "blue"))
-- "pink mrow haha mrow green mrow woops mrow blue"

-- 6. cattyConny (flippy "Pugs" "are") "awesome" = "are mrow Pugs mrow awesome"


-- Recursion
-- 1. Write out the steps for reducing dividedBy 15 2 to its final answer

-- dividedBy 15 2 =
--   go 15 2 0

--   | go (15-2) 2 (0+1)
--   | go 13 2 1

--   | go (13-2) 2 (1+1)
--   | go 11 2 2

--   | go (11-2) 2 (2+1)
--   | go 9 2 3

--   | go (9-2) 2 (3+1)
--   | go 7 2 4

--   | go (7-2) 2 (4+1)
--   | go 5 2 5

--   | go (5-2) 2 (5+1)
--   | go 3 2 6

--   | go (3-2) 2 (6+1)
--   | go 1 2 7

--   | 1 < 2 = (7, 1)

--   (7, 1)

-- 2. Write a function that recursively sums all numbers from 1 to n, n being the arg
-- If n was 5, you'd add 1 + 2 + 3 + 4 + 5 to get 15.
-- The type should be (Eq a, Num a) => a -> a.
sumToN :: (Ord a, Eq a, Num a) => a -> a --modifying constraints to account for -ve nums
sumToN n
  | n == 0 = 0
  | n > 0  = n + sumToN (n-1)
  | otherwise = n + sumToN (n+1)

-- 3. Write a function that multiplies two integral numbers using recursive summation.
-- The type should be (Integral a) => a -> a -> a.
mult :: Integral a => a -> a -> a
-- mult 0 int = 0
-- mult n int = int + mult (n-1) int
mult 0 = const 0
mult n = \i -> i + mult (n-1) i


-- Fixing dividedBy
-- see dividedBy.hs


-- McCarthy 91 function
mc91 :: Integral a => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 (mc91 (n + 11))


-- Numbers into words
-- see wordNumber.hs
