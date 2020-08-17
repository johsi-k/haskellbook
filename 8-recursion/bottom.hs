-- we get an exception on f True because we specified that
-- the value should return an error
f :: Bool -> Int
f True = error "blah"
f False = 0

-- partial function
-- that elides f True = error "blah"
-- this gives a different exception: non-exhaustive patterns
f' :: Bool -> Int
f' False = 0

-- actually:
-- f' :: Bool -> Int
-- f' False = 0
-- f' _ = error $ "*** Exception: "
--             ++ "Non-exhaustive"
--             ++ "patterns in function f"

-- how can we make f a total function?
-- one way is to use the Maybe datatype
-- data Maybe a = Nothing | Just a
-- see brokenMaybe1.hs
