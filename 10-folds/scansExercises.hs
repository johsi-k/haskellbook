fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Int -> Integer
fibsN x = fibs !! x

-- 1. Modify fibs to return only the first 20 Fibonacci numbers
fibs20 :: [Integer]
fibs20 = take 20 (1 : scanl (+) 1 fibs20)


-- 2. Modify fibs to return the Fibonacci numbers that are less than 100
fibsUnder100 :: [Integer]
fibsUnder100 = takeWhile (<100) (1 : scanl (+) 1 fibs)


-- 3. Rewrite the factorial function from Recursion as a scan. You'll want scanl again, and your start value will be 1.
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

fact :: Integer -> [Integer]
fact n = scanl (*) 1 [2..n]
