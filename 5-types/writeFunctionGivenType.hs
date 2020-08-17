-- 1.
i :: a -> a
i x = x

-- 2.
c :: a -> b -> a
c x _ = x

-- 3.
c'' :: b -> a -> b
c'' = c

-- 4.
c' :: a -> b -> b
c' _ y = y

-- 5.
r :: [a] -> [a]
r [] = []
r (_:xs) = xs

-- r list = list
-- r list = reverse list

-- 6.
co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB ay = bToC (aToB ay)

-- 7.
a :: (a -> c) -> a -> a
a _ ay = ay

-- 8.
a' :: (a -> b) -> a -> b
-- a' aToB ay = aToB ay
a' aToB = aToB -- by eta reduction, which gives the identity function!

data Flour = Flour
data Dough = Dough
data Bread = Bread

-- to make bread now
makeBread :: (Dough -> Bread) -- bake
          -> (Flour -> Dough) -- addWater
          -> Flour
          -> Bread
makeBread bake addWater flour = bake (addWater flour)

-- as a blueprint for breadmaking
type Bakery = Flour -> Bread
bakeryFactory :: (Dough -> Bread) -- bake
              -> (Flour -> Dough) -- addWater
              -> Bakery -- (Flour -> Bread)
bakeryFactory bake addWater = bake . addWater
