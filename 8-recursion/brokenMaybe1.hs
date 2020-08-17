f :: Bool -> Maybe Int
f False = 0
f _ = Nothing

-- error arises because 0 has type Num a => a
-- so it's trying to get an instance of Num for Maybe Int
