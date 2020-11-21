-- foldlar = foldr combine base "abc"
--   where
--     base = const []
--     combine x fn r
--       | null next = x : r
--       | otherwise = next
--       where next = fn (x : r)

-- desugared
foldlar' = foldr combine base "abc"
  where
    base = const []
    combine x f = \r ->
      case null (f (x : r)) of
        True -> x : r
        _    -> f (x : r)

-- evaluating:
-- combine 'a' (combine 'b' (combine 'c' (foldr combine base [])))
-- = combine 'a' (combine 'b' (combine 'c' (const [])))

-- combine 'c' (const [])
-- = \r -> case null ((const []) ('c' : r)) of
--   True -> 'c' : r
--   _    -> (const []) ('c' : r)
-- = \r -> case null [] of
--   True -> 'c' : r
--   _    -> []
-- = \r -> 'c' : r

-- combine 'b' (\r' -> 'c' : r')
-- = \r -> case null ((\r' -> 'c' : r') ('b' : r)) of
--   True -> 'b' : r
--   _    -> (\r' -> 'c' : r') ('b' : r)
-- = \r -> case null ('c' : 'b' : r) of
--   True -> 'b' : r
--   _    -> ('c' : 'b' : r)
-- = \r -> ('c' : 'b' : r)

-- combine 'a' (\r' -> ('c' : 'b' : r'))
-- = \r -> case null ((\r' -> ('c' : 'b' : r')) ('a' : r)) of
--   True -> 'a' : r
--   _    -> (\r' -> ('c' : 'b' : r')) ('a' : r)
-- = \r -> case null ('c' : 'b' : 'a' : r) of
--   True -> 'a' : r
--   _    -> ('c' : 'b' : 'a' : r)
-- = \r -> ('c' : 'b' : 'a' : r)

-- suspicious: first value is treated differently and has to be special-cased inside the reducing function


-- a better way
foldlar = foldr combine base "abc"
  where
    base = id
    combine x f = \r -> f (x : r)

-- evaluating:
-- combine 'a' (combine 'b' (combine 'c' (foldr combine id "")))
-- = combine 'a' (combine 'b' (combine 'c' id))

-- combine 'c' id
-- = \r -> id ('c' : r)
-- = \r -> ('c' : r)

-- combine 'b' (\r' -> ('c' : r'))
-- = \r -> (\r' -> ('c' : r')) ('b' : r)
-- = \r -> ('c' : 'b' : r)

-- combine 'a' (\r' -> ('c' : 'b' : r'))
-- = \r -> (\r' -> ('c' : 'b' : r')) ('a' : r)
-- = \r -> ('c' : 'b' : 'a' : r)
