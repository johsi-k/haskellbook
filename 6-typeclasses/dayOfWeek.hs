data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun
  deriving Show

-- day of week and numerical day of month
data Date =
  Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon   = True
  (==) Tue Tue   = True
  (==) Weds Weds = True
  (==) Thu Thu   = True
  (==) Fri Fri   = True
  (==) Sat Sat   = True
  (==) Sun Sun   = True
  (==) _ _       = False

-- dates are equal when all their constituent values are equal
instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
    weekday == weekday'
      && dayOfMonth == dayOfMonth'

-- to declare that Friday is always the best day
-- ensure that Ord instances agree with Eq instances
instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _ = GT
  compare _ Fri = LT
  compare _ _ = EQ


data Heh = Heh
instance Show Heh where
  -- show :: a -> String
  show _ = "Heh"
