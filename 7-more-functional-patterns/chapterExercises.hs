-- 1.
-- A polymorphic function
-- d) may resolve to values of different types, deepending on inputs

-- 2.
-- Two functions named f and g have types Char -> String and String -> [String]
-- respectively. The composed function g . f has the type
-- b) Char -> [String]

-- 3.
-- A function f has the type Ord a => a -> a -> Bool and
-- we apply it to one numeric value. What is the type now?
-- d) (Ord a, Num a) => a -> Bool

-- 4.
-- A function with the type (a -> b) -> c
-- b) is a higher-order function

-- 5.
-- Given the following definition of f, what is the type of f True?
f :: a -> a
f x = x
-- a) f True :: Bool


-- Let's write code
-- 1.
-- The following function returns the tens digit of an integral argument.
tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

-- a) rewrite it using divMod
tensDigit' :: Integral a => a -> a
-- tensDigit' x = fst (x `divMod` 10) `mod` 10
tensDigit' = (`mod` 10) . fst . (`divMod` 10)

tensDigit'' :: Integral a => a -> a
tensDigit'' = snd . (`divMod` 10) . (`div` 10)

-- b)
-- The divMod version has the same type as the original

-- c)
-- Let's change it so we're getting the hundreds digit instead.
-- You could start like this:
-- hunsD x = d2
--   where d = undefined

hunsD :: Integral a => a -> a
hunsD = (`mod` 10) . fst . (`divMod` 100)

-- 2.
-- Implement the function of type a -> a -> Bool -> a once each
-- using a case expression and once with a guard.
-- foldBool :: a -> a -> Bool -> a
-- foldBool =
  -- error
  -- "Error: Need to implement foldBool!"

foldBool :: a -> a -> Bool -> a
foldBool x y bool = if bool then y else x

foldBool1 :: a -> a -> Bool -> a
foldBool1 x y bool =
  case bool of
    False -> x
    True -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y bool
  | bool = y
  | otherwise = x

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True  = y


-- 3.
g :: (a -> b) -> (a, c) -> (b, c)
g aToB (a, c) = (aToB a, c)

-- 4.
-- see arith4.hs
