-- Will it blow up?

-- Will the following expressions return a value or be ⊥?
-- remember referential transparency

-- 1. [x^y | x <- [1..5], y <- [2, undefined]]
-- blows up because evaluation of all values is forced

-- 2. take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]
-- returns [1]

-- 3. sum [1, undefined, 3]
-- blows up because evaluation of all values is forced

-- 4. length [1, 2, undefined]
-- returns 3

-- 5. length $ [1, 2, 3] ++ undefined
-- blows up because part of spine is bottom

-- 6. take 1 $ filter even [1, 2, 3, undefined]
-- returns [2]

-- 7. take 1 $ filter even [1, 3, undefined]
-- blows up because evaluation of all values is forced

-- 8. take 1 $ filter odd [1, 3, undefined]
-- returns [1]

-- 9. take 2 $ filter odd [1, 3, undefined]
-- returns [1, 3]

-- 10. take 3 $ filter odd [1, 3, undefined]
-- blows up because evaluation of all values is forced


-- Intermission: Is it normal form?
-- Determine if each expression is in:
-- Normal Form (which implies WHNF)
-- Weak Head Normal Form only or
-- neither

-- _ is a placeholder for ''not evaluated yet'

-- 1. [1, 2, 3, 4, 5]
-- WHNF and NF

-- 2. 1 : 2 : 3 : 4 : _
-- WHNF
-- can be rewritten (:) 1 (2 : 3 : 4 : _)

-- 3. enumFromTo 1 10
-- neither

-- 4. length [1, 2, 3, 4, 5]
-- neither

-- 5. sum (enumFromTo 1 10)
-- neither

-- 6. ['a'..'m'] ++ ['n'..'z']
-- neither

-- 7. (_, 'b')
-- WHNF
-- not NF since _ can be further evaluated
