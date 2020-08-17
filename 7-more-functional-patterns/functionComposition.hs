-- Function composition allows us to combine functions
-- such that the result of applying one function gets passed
-- to the next function as an argument

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- the result of (a -> b) is the argument of (b -> c),
-- which is how we go from arg a to result c

-- basic syntax of function composition
-- (f . g) = f (g x)
-- the (.) or composition operator pipelines data through multiple functions

-- > negate . sum $ [1, 2, 3, 4, 5]
-- > -15
-- > or: (negate . sum) [1, 2, 3, 4, 5]

-- evaluates to:
-- negate (sum [1, 2, 3, 4, 5])
-- negate (15)
-- -15

-- $ signals to that the function application to the arguments
-- should happen after the functions are composed
-- It is needed because function application has a higher precedence than function composition
-- and therefore happens before function composition by default:
-- > negate . sum [1, 2, 3, 4, 5]
-- > negate . 15

-- > take 5 . reverse $ [1..10]
-- > [10, 9, 8, 7, 6]

-- > take 5 (enumFrom 3)
-- > take 5 . enumFrom $ 3

f :: Num a => [a] -> a -- this somehow cannot be omitted from a source file
f = negate . sum
-- > f [1, 2, 3, 4, 5]
-- > -15

-- rewriting as pointfree function
-- g :: Int -> [Int] -> Int
-- g z xs = foldr (+) z xs
g :: Num a => a -> [a] -> a -- this produces an error when omitted
g = foldr (+)
-- g is just a partially applied function here?

filterA = length . filter (== 'a')
