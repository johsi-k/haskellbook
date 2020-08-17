-- options:
-- fully polymorphic type variable
-- constrained polymorphic type variable
-- concrete type constructor

f :: Num a => a -> b -> Int -> Int
--           [0]  [1]   [2]    [3]
-- [0]: constrained polymorphic
-- [1]: fully polymorphic
-- [2]: concrete type
-- [3]: concrete type

f :: zed -> Zed -> Blah
--   [0]    [1]    [2]
-- [0]: fully polymorphic 
-- [1]: concrete
-- [2]: concrete

f :: Enum b => a -> b -> C
--            [0]  [1]  [2]
-- [0]: fully polymorphic
-- [1]: constrained polymorphic
-- [2]: concrete

f :: f -> g -> C
--  [0]  [1]  [2]
-- [0]: fully polymorphic 
-- [1]: fully polymorphic
-- [2]: concrete