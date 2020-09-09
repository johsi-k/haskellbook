-- lists are a recursive series of cons cells a : [] terminated by the empty list []
-- a spine is a connective structure that ties the collection of values together
-- with a list, the spine is textually represented by the recursive cons operator
-- 1 : 2 : 3 : []
-- 1 : (2 : (3 : []))
--   : <------|
--  / \       |
-- 1   : <----| This is the "spine"
--    / \     |
--   2   : <--|
--      / \
--      3  []

-- in lists,
-- evaluation proceeds down the spine, beginning with 1 and new cons cell
-- construction proceeds up the spine, beginning with putting 3 into the empty list

-- because of Haskell's nonstrict evaluation
-- the list isn't constructed until it's consumed


-- GHCi's :sprint command
blah = enumFromTo 'a' 'z'
-- :sprint blah
-- > blah = _
-- blah is totally unevaluated

-- > take 1 blah
-- > "a"
-- > :sprint blah
-- > 'a' : _
-- this forces the evaluation of the first cons cell and the first value 'a'

-- spine strictness:
-- only forces evaluation of the spine of a list, not the values
-- e.g. length function
-- evident with length [1, undefined] = 2
-- but :sprint will behave as though the evaluation of values is forced (quirk!)

-- Weak Head Normal Form
-- a set containing both the possibility that the expr
-- 1. is fully evaluated (normal form)
-- 2. has been evaluated to the point of arriving at a data constructor / lambda awaiting an argument

-- examples
-- (1, 2) -- WHNF & NF

-- (1, 1 + 1) -- WHNF
-- WHNF because we have a (,) data constructor
-- not NF because (+) has been applied to args but not evaled

-- \x -> x * 10 -- WHNF & NF
-- WHNF because we have a lambda expression
-- NF because it cannot be reduced until outer lambda has been applied

-- "Papu" ++ "chon" -- neither WHNF nor NF
-- args are fully applied but not evaluated

-- (1, "Papu" ++ "chon") -- WHNF

-- when defining a list and enumerating its values, it is in NF
-- when constructing a list through ranges/functions, it is in WHNF but not NF

-- _ to ignore values in args
-- also signals to the compiler that it won't need to eval something in that case
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + length xs

--       :       <-|
--      / \        |
-- |-> _   :     <-|
-- |      / \      | These got evaluated (forced)
-- |->   _   :   <-|
-- |        / \    |
-- |->     _  [] <-|
-- |
-- | These did not

-- length will throw an error if part of the spine itself is bottom
x = [1] ++ undefined ++ [3]

-- functions that force both spine and values
mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs
-- (+) is strict in both its arguments, forcing evaluation of the values and mySum xs
