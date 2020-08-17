fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

-- fibonacci 6 = fibonacci 5 + fibonacci 4
-- fibonacci 5 = fibonacci 4 + fibonacci 3
-- fibonacci 4 = fibonacci 3 + fibonacci 2
-- fibonacci 3 = fibonacci 2 + fibonacci 1
-- fibonacci 2 = fibonacci 1 + fibonacci 0

-- fibonacci 0 +             0
-- fibonacci 1 +             1
-- fibonacci 2 +   (1 + 0 =) 1
-- fibonacci 3 +   (1 + 1 =) 2
-- fibonacci 4 +   (2 + 1 =) 3
-- fibonacci 5 +   (3 + 2 =) 5
-- fibonacci 6 +   (5 + 3 =) 8
