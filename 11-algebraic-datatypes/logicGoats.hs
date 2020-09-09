{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
-- newtype: a type that can only ever have a single unary data constructor

-- tooManyGoats :: Int -> Bool
-- tooManyGoats n = n > 42
-- presents a problem when we have different limits for different livestock

newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)

newtype Cows =
  Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

-- using newtype confers other advantages related to typeclass instances
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42
-- to use this we need to assign the type Int to the numeric literal we're passing in because numeric literals are polymorphic

-- instance TooMany Goats where
  -- tooMany (Goats n) = n > 43
  -- tooMany (Goats n) = tooMany n -- if without GeneralizedNewtypeDeriving pragma
-- no need to assign a type to numeric literals here
-- tooMany :: TooMany a => a -> Bool

-- the Goats newtype can have an instance of TooMany with different behaviour than the type Int
-- this cannot happen if Goats were a type synonym
-- type Goats' = Int

-- instance TooMany Goats' where
--   tooMany n = n > 43
-- "illegal instance declaration: all instance types must be of the form (T t1 ... tn) where T is not a synonym"


-- What happens when we want to reuse the typeclass instances of the type our newtype contains - the way we do with common typeclasses like Eq and Show?
-- see second line in the Goats instance
-- or we could add the pragma GeneralizedNewtypeDeriving

-- The language extension GeneralizedNewtypeDeriving tells the compiler to allow newtype to rely on a typeclass instance for the type it contains


-- Exercises: Logic Goats
-- 1. Reusing the TooMany typeclass, write an instance of the typeclass for the type (Int, String). This will require adding a language pragma called FlexibleInstances if you do not use a newtype - GHC will tell you what to do.
newtype IntString = IntString' (Int, String)

instance TooMany IntString where
  tooMany (IntString' (int, _)) = tooMany int

-- with FlexibleInstances
instance TooMany (Int, String) where
  tooMany (int, _) = tooMany int


-- 2. Make another TooMany instance for (Int, Int). Sum the values together under the assumption this is a count of goats from two fields.
instance TooMany (Int, Int) where
  tooMany (int1, int2) = tooMany (int1 + int2)


-- 3. Make another TooMany instance, this time for (Num a, TooMany a) => (a, a). This can mean whatever you want, such as summing the two numbers together.
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n1, n2) = tooMany (n1 + n2)
