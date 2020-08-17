-- if we clarify our intent a bit ...
f :: Bool -> Maybe Int
f False = 0 :: Int
f _ = Nothing

-- we get a better type error
-- couldn't match expected type 'Maybe Int' with actual type 'Int'
