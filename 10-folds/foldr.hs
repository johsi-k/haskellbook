
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- map :: (a -> b) -> [a] -> [b]

-- map applies a function to each member of a list and returns a list
-- map (+1) 1 :      2 :      3 : []
-- map (+1) 1 : (+1) 2 : (+1) 3 : []

-- foldr replaces the cons constructors with the function and reduces the list
-- foldr (+) 0 (1 :  2 : 3  : [])
--              1 + (2 + (3 +  0))

-- in each of the following, the base case is the identity for that function
-- addition: adding 0 always gives the same result as the initial value
-- multiplication: multiplying 1 always gives the same result
-- concatenation: concatenating something with an empty list always returns that thing

-- sum :: [a] -> Integer
-- sum []     = 0
-- sum (x:xs) = x + sum xs

-- length :: [a] -> Integer
-- length []     = 0
-- length (x:xs) = 1 + length xs

-- product :: [Integer] -> Integer
-- product []     = 1
-- product (x:xs) = x * product xs

-- concat :: [[a]] -> [a]
-- concat []     = []
-- concat (x:xs) = x ++ concat xs

-- Fold right
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f z [] = z
-- foldr f z (x:xs) = f x (foldr f xs)


-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f z xs =
--   case xs of
--     []     -> z
--     (x:xs) -> f x (foldr f z xs)

-- evaluating foldr (+) 0 [1, 2, 3]
-- foldr (+) 0 [1, 2, 3] =
--   case [1, 2, 3] of
--     []           -> 0 -- not this
--     (1 : [2, 3]) -> (+) 1 (foldr (+) 0 [2, 3])

-- we expand (foldr (+) 0 [2, 3]) only because (+) is strict in both its args (and unconditionally so)
-- the function passed to foldr does not necessarily continually force the rest of the fold
-- (+) 1 from the earlier iteration is implicitly wrapped around the next step of the fold
-- foldr (+) 0 [2, 3] =
--   case [2, 3] of
--     []        -> 0 -- not this
--     (2 : [3]) -> (+) 2 (foldr (+) 0 [3])

-- (+) 1 ((+) 2 ...) is implicitly wrapped around this next step of the fold
-- foldr (+) 0 [3] =
--   case [3] of
--     []       -> 0 -- not this
--     (3 : []) -> (+) 3 (foldr (+) 0 [])

-- (+) 1 ((+) 2 ((+) 3 ...)) is implicitly wrapped around this next step of the fold
-- foldr (+) 0 [] =
--   case [] of
--     [] -> 0 -- this matches!

-- putting everything together:
-- (+) 1 ((+) 2 ((+) 3 0)) = 6

-- to visualise how the fold associates
-- xs = map show [1..3]
-- ["1","2","3"]
-- y = foldr (\x y -> concat ["(", x, "+", y, ")"]) "0" xs
-- > y
-- "(1+(2+(3+0)))"

-- foldr (\x b -> even x || b) False [1, 2, 3] =
--   case [1, 2, 3] of
--     []           -> False
--     (1 : [2, 3]) -> (\b -> even 1 || b) (foldr (\x b -> even x || b) False [2, 3])

-- foldr (\x b -> even x || b) False [2, 3] =
--   case [2, 3] of
--     []       -> False
--     (2: [3]) -> (\b -> even 2 || b) (foldr (\x b -> even x || b) False [3])

-- foldr (\x b -> even x || b) False [3] =
--   case [3] of
--     []      -> False
--     (3: []) -> (\b -> even 3 || b) (foldr (\x b -> even x || b) False [])

-- foldr (\x b -> even x || b) False [] =
--   case [ ] of
--     [] -> False

-- (\b -> even 1 || b) $ (\b -> even 2 || b) $ (\b -> even 3 || b) False

-- (\b -> False || b) $ (\b -> True || b) $ (\b -> False || b) False


-- map (*2) (map (+1) [1,2,3]) -- Expand the definition of the first map
--   = case (map (+1) [1,2,3]) of -- Now we are forcing ys
--          [] -> []
--          (x : xs) -> (*2) x : map (*2) xs
--   = let ys =
--     in case (case [1,2,3] of { -- So we expand the definition of the second map too
--                [] -> [];
--                (x':xs') -> (+1) x' : map (+1) xs';
--             }) of
--          [] -> []
--          (x : xs) -> (*2) x : map (*2) xs
--   = -- Evaluate the first case expression
--     case ((+1) 1 : map (+1) [2,3]) of
--          [] -> []
--          (x : xs) -> (*2) x : map (*2) xs
--   =                 -- Evaluate the second case expression
--     (*2) ((+1) 1)  : map (*2) (map (+1) [2,3])

-- take
-- > xs = ([1, 2] :: [Int]) ++ undefined
-- > :sprint xs
-- xs = _
-- > length $ take 2 $ take 4 xs
-- 2
 -- > :sprint xs
-- xs = 1 : 2 : _

