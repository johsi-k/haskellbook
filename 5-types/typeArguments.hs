-- 1.
f :: a -> a -> a -> a
f = undefined

x :: Char
x = undefined

-- f x :: Char -> Char -> Char

-- 2.
g :: a -> b -> c -> b
g = undefined

-- g 0 'c' "woot" :: Char


-- 3.
h :: (Num a, Num b) => a -> b -> b
h = undefined

-- h 1.0 2 :: Num b => b

-- 4.
-- h 1 (5.5 :: Double) :: Double

-- 5.
jackal :: (Ord a, Eq b) => a -> b -> a
jackal = undefined
-- jackal "keyboard" "has the word jackal in it" :: [Char]

-- 6.
-- jackal "keyboard" :: Eq b => b -> [Char]

-- 7.
kessel :: (Ord a, Num b) => a -> b -> a
kessel = undefined

-- kessel 1 2 :: (Num a, Ord a) => a

-- 8.
-- kessel 1 (2 :: Integer) :: (Num a, Ord a) => a 

-- 9.
-- kessel (1 :: Integer) 2 :: Integer