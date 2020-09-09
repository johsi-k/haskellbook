
-- map  ::              (a -> b) -> [a] -> [b]
-- fmap :: Functor f => (a -> b) -> f a -> f b

-- map (+1) :: Num b => [b] -> [b]
-- fmap (+1) :: (Functor f, Num b) => f b -> f b

-- how map is defined in base:
-- map :: (a -> b) -> [a] -> [b]
-- map _ [] = []
-- map f (x:xs) = f x : map f xs

-- an idea of how map works (not representative of nonstrict evaluation)
-- map (+1) [1, 2, 3]
-- map (+1) (1 : (2 : (3 : [])))

-- not an empty list; second pattern fires
-- (+1) 1 :
--   map (+1)
--     (2 : (3 : []))

-- (+1) 1 :
--   ((+1) 2 :
--     (map (+1)
--       (3 : [])))

-- (+1) 1 :
--   ((+1) 2 :
--     ((+1) 3 :
--       (map (+1) : [])))

 -- emepty list triggers base case
-- (+1) 1 :
--   ((+1) 2 :
--     ((+1) 3 : []))

-- reducing,
-- 2 : ((+1) 2 : ((+1) 3 : []))
-- 2 : (3 : ((+1) 3 : []))
-- 2 : (3 : (4 : []))
-- [2, 3, 4]

-- using the syntactic sugar of list
-- map f [1, 2, 3] == [f 1, f 2, f3]
-- map (+1) [1, 2, 3]
--          [(+1) 1, (+1) 2, (+1) 3]
--          [2, 3, 4]

-- map does not traverse the list and apply the function immediately:
-- map (+1) [1, 2, undefined]
-- > [2,3,*** Exception: Prelude.undefined

-- take 2 $ map (+1) [1, 2, undefined]
-- > [2, 3]
-- no error because take 2 was used to request only the first two elements
-- we only force as many values as cons cells we forced (2)
-- values are forced only if we evaluate the result value in the list that map returns
