import Control.Applicative

boop :: Integer -> Integer
boop = (*2)

doop :: Integer -> Integer
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (.) :: (Integer -> Integer)
--     -> (Integer -> Integer)
--     -> Integer -> Integer

bloop :: Integer -> Integer
bloop = fmap boop doop

-- fmap :: (a -> b) -> f a -> f b
-- fmap :: (Integer -> Integer)
--      -> (->) Integer Integer
--      -> (->) Integer Integer


bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

-- fmap :: (Integer -> Integer -> Integer)
--      -> (->) Integer Integer
--      -> (->) Integer (Integer -> Integer)

-- (<*>) :: (->) Integer (Integer -> Integer)
--       -> (->) Integer Integer
--       -> (->) Integer Integer

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- liftA2 :: (Integer -> Integer -> Integer)
--        -> (->) Integer Integer
--        -> (->) Integer Integer
--        -> (->) Integer Integer

-- The fmap bit
-- f . g = \x -> f (g x)
-- \x -> (+) ((*2) x) 5 3
-- \5 -> (+) ((*2) 5) 3
-- ((+) 10) 3
-- 13

-- ((+) . (*2)) 5 3
-- ((\a b -> a+b) . (\c -> c*2)) 5 3
-- ((\a b -> a+b) ((\c -> c*2) 5)) 3
-- ((\a b -> a+b) (5*2)) 3
-- ((\a b -> a+b) 10) 3
-- 10 + 3
-- 13

-- The applicative bit
-- ((+) <$> (*2) <*> (+10)) 3
-- (((+) . (*2)) <*> (+10)) 3
-- (((\a b -> a+b) . (\c -> c*2)) <*> (+10)) 3
-- ((\a b -> a+b) . (\c -> c*2)) 3 ((+10) 3)
-- ((\a b -> a+b) ((\c -> c*2) 3)) ((+10) 3)
-- ((\a b -> a+b) 6) (13)
-- 6+13
-- 19

-- The intuition
-- We feed a single argument to (*2) and (+10), and the two results form the two arguments to (+)
-- ((+) <$> (*2) <*> (+10)) 3
-- (3*2) + (3*10)
-- 6 + 13
-- 19

-- More examples
-- λ> liftA2 (++) ('b':) ('d':) $ "oop"
-- "boopdoop"

-- λ> liftA2 (++) (:"eep") (:"oop") 'm'
-- "meepmoop"
-- λ> liftA2 (++) (:"eep") (:"oop") 'b'
-- "beepboop"
-- λ> liftA2 (++) (:"eep") (:"oop") 'd'
-- "deepdoop"

-- λ> liftA2 (++) (:"eep") (:"oop") <$> "mbd"
-- ["meepmoop","beepboop","deepdoop"]
