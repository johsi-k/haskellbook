dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

-- 1.
-- > dodgy 1 0
-- > 1

-- 2.
-- > dodgy 1 1
-- > 11

-- 3.
-- > dodgy 2 2
-- > 22

-- 4.
-- > dodgy 1 2
-- > 21

-- 5.
-- > dodgy 2 1
-- > 12

-- 6.
-- > oneIsOne 1
-- > 11

-- 7.
-- > oneIsOne 2
-- > 21

-- 8.
-- > oneIsTwo 1
-- > 21

-- 9.
-- > oneIsTwo 2
-- > 22

-- 10.
-- > oneIsOne 3
-- > 31

-- 11.
-- > oneIsTwo 3
-- > 23
