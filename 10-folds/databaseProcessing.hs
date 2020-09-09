import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
           (fromGregorian 1911 5 1)
           (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world"
  , DbDate (UTCTime
           (fromGregorian 1921 5 1)
           (secondsToDiffTime 34123))
  ]

-- 1. Write a function that filters for DbDate values and returns a list of the UTCTime values inside them.
filterDbDate :: [DatabaseItem]
             -> [UTCTime]
-- filterDbDate = foldr f []
  -- where f dbItem acc =
  --         case dbItem of
  --           DbDate time    -> time : acc
  --           _              -> acc
  -- where
  --   f dbItem acc
  --     | DbDate time <- dbItem = time : acc
  --     | otherwise = acc
  -- where
  --   f (DbDate time) = (time :)
  --   f _             = id -- \acc -> acc
filterDbDate dbs = [time | DbDate time <- dbs]


-- 2. Write a function that filters for DbNumber values and returns a list of the Integer vaues inside them.
filterDbNumber :: [DatabaseItem]
               -> [Integer]
filterDbNumber = foldr f []
  where
    f (DbNumber int) = (int :)
    f _              = id


-- 3. Write a function that gets the most recent date
mostRecent :: [DatabaseItem]
           -> UTCTime
-- mostRecent = maximum . filterDbDate

-- mostRecent = foldr f (UTCTime (ModifiedJulianDay 0) 0)
--   where
--     f (DbDate time) = max time
--     f _             = id

mostRecent dbs = case [time | (DbDate time) <- dbs] of
  []     -> error "no DbDates found"
  (x:xs) -> foldr max x xs


-- 4. Write a function that sums all the DbNumber values
sumDb :: [DatabaseItem]
      -> Integer
-- sumDb = sum . filterDbNumber
sumDb dbs = case [int | (DbNumber int) <- dbs] of
  []     -> error "no DbNumbers found"
  (x:xs) -> foldr (+) x xs


-- 5. Write a function that gets the average of the DbNumber values
avgDb :: [DatabaseItem]
      -> Double
-- avgDb xs = fromIntegral (sum (filterDbNumber xs)) / fromIntegral (length (filterDbNumber xs))
avgDb xs = (fromIntegral . sumDb $ xs) / (fromIntegral . length . filterDbNumber $ xs)
