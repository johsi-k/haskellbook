{-# LANGUAGE InstanceSigs #-}
import Control.Applicative (liftA2)

-- Breaking down the Functor of functions

-- instance Functor ((->) r) where
--   fmap = (.)

-- data (->) a b

-- type constructor of functions
-- (->)
-- fully applied
-- a -> b

-- ((->) r) is
-- r ->

-- r is the type of the argument to the function
-- the argument type for functions is part of the structure being lifted over
-- this leaves the result of the function as the value being transformed


-- Reader newtype
newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) =
    -- Reader $ \r -> f (ra r)
    Reader (f . ra)

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

-- these are the same
-- \r -> f (ra r)
-- \x -> f (g x)

-- In the Reader functor,
-- ra :: r -> a
-- f :: a -> b
-- we have function composition!

-- Exercise: Ask
-- see exercises.hs


-- Functions have an Applicative, too

-- Applicative f =>
-- f ~ (->) r

-- pure :: a -> f a
-- pure :: a -> (r -> a)

-- (<*>) ::  f   (a -> b) ->  f    a  ->  f    b
-- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)


-- Demonstrating the function Applicative
newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person {
      humanName :: HumanName
    , dogName :: DogName
    , address :: Address
    } deriving (Eq, Show)

data Dog =
  Dog {
      dogsName :: DogName
    , dogsAddress :: Address
    } deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Sesame Street")

chris :: Person
chris =
  Person (HumanName "Chris Allen")
         (DogName "Papu")
         (Address "Austin")

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

-- <$> :: (a -> b)
--     -> (r -> a)
--     -> (r -> b)

-- <$> :: (DogName -> (Address -> Dog))
--     -> (Person -> DogName)
--     -> (Person -> (Address -> Dog))

-- <*> :: (r -> a -> b)
--     -> (r -> a)
--     -> (r -> b)

-- <*> :: (Person -> (Address -> Dog))
--     -> (Person -> Address)
--     -> (Person -> Dog)

getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

-- liftA2 :: Applicative f
--        => (a -> b -> c)
--        -> f a -> f b -> f c

-- liftA2 :: (a -> b -> c)
--        -> (r -> a)
--        -> (r -> b)
--        -> (r -> c)

-- liftA2 :: (DogName -> Address -> Dog)
--        -> (Person -> Dogname)
--        -> (Person -> Address)
--        -> (Person -> Dog)


-- Exercise: Reading comprehension
-- see exercises.hs


-- The Monad of functions
-- (>>=) :: ((->) r a) -> ((a -> (->) r b) -> ((->) r b)
-- (>>=) :: (r -> a)   -> (a -> (r -> b))  -> (r -> b)

-- Example uses of the Reader type
getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy


-- Exercise: Reader Monad
-- see exercises.hs

-- Generally we cannot derive a Monad instance from the Applicative. But here our instances are specifically the type of functions so we can use flip and apply to make the Monad instance.

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- instance Functor ((->) r) where
--   fmap = (.)

-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- instance Applicative ((->) r) where
--   pure = const
--   f <*> a = \r -> f r (a r)

-- class Applicative f => Monad f where
--   return :: a -> f a
--   (>>=) :: f a -> (a -> f b) -> f b

-- instance Monad ((->) r) where
--   return = pure
--   m >>= k = flip k <*> m
