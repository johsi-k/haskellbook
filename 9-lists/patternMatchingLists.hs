myHead :: [a] -> a
myHead (x: _) = x

myTail :: [a] -> [a]
myTail (_ : xs) = xs

-- but neither myHead nor myTail can handle empty lists

-- we could add a base case
myTail' :: [a] -> [a]
myTail' [] = []
myTail' (_ : xs) = xs

-- a better way: Maybe
-- data Maybe = Nothing | Just a
safeTail :: [a] -> Maybe [a]
safeTail []       = Nothing
safeTail [_]      = Nothing
safeTail (_ : xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

-- syntactic sugar
-- [1, 2, 3] ++ [4] == (1 : 2 : 3 : []) ++ 4 : []

-- range and enum equivalents
-- [1..10] generates [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
-- desugared: enumFromTo 1 10

-- [1, 3..10] generates [1,3,5,7,9]
-- desugared: enumFromThenTo 1 3 10

-- enumFrom :: Enum a => a -> [a]
-- enumFromThen :: Enum a => a -> a -> [a]
-- enumFromTo :: Enum a => a -> a -> [a]
-- enumFromThenTo :: Enum a => a -> a -> a -> [a]
