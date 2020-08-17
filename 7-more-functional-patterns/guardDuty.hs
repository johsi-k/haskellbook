-- The function below translates the number of
-- correctly answered questions out of 100 to a letter grade

avgGrade :: (Fractional a, Ord a)
         => a -> Char
avgGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  -- | y < 0.59 = 'F'
  | otherwise = 'F'
  where y = x / 100

-- less than was used instead of otherwise in the final case
-- which is ok when the guards have handled all values
-- but it is always safer to use otherwise

-- 1.
-- What happens when otherwise is used as the top-most guard
-- and you pass in 90 as an argument? 75? 60?
-- all inputs would yield 'F' since the guard is never False

-- 2.
-- What happens when you take avgGrade as written and reorder the guards?
-- try moving the 'C' guard and passing in 90 as an argument
-- does it return an 'A'?
-- no, since the 'C' guard is more permissive than that of 'A'

-- 3.
-- The following function returns
pal xs
  | xs == reverse xs = True
  | otherwise        = False
 -- b) True when xs is a palindrome

-- 4.
-- What types of arguments can pal take?
-- lists whose elements have an instance of Eq

-- 5.
-- What is the type of the function pal?
pal :: Eq a => [a] -> Bool

-- 6.
-- The following function returns
numbers x
  | x < 0  = -1
  | x == 0 = 0
  | x > 0  = 1
-- c) an indication of whether its argument is a positive or negative number or zero

-- 7.
-- What types of arguments can numbers take?
-- numbers takes arguments that have instances of Num, Ord and Eq

-- 8.
-- What is the type of the function numbers?
numbers :: (Num a, Eq a, Ord a) => a -> a
