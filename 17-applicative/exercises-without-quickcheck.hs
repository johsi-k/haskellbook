import Data.List (elemIndex)

-- Exercises: Lookups
-- In the following exercises you will need to use the following terms to make the expressions typecheck:

-- 1. pure

-- 2. (<$>) or fmap

-- 3. (<*>)

-- Make the following expressions typecheck

-- 1.
added :: Maybe Integer
added = fmap (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- zip :: [a] -> [b] -> [(a, b)]

-- a = 3
-- lookup :: (Eq a, Num a) => [(a, b)] -> Maybe b

-- [(a, b)] = zip [1, 2, 3] [4, 5, 6]
-- lookup :: (Eq a, Num a) => Maybe b

-- (+3) :: Num a => a -> a
-- (+3) :: Integer -> Integer

-- fmap :: Num a => (a -> a) -> Maybe a -> Maybe a


-- 2.
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- (,) :: a -> b -> (a, b)
-- ? :: Maybe Integer -> Maybe Integer -> Maybe (Integer, Integer)

-- fmap :: Functor f => (x -> y) -> f x -> f y

-- in context: applying fmap (,) and Maybe Integer,
-- x = a, y = b -> (a, b), f = Maybe, x = Integer, y = Integer
-- fmap :: (Integer -> (Integer -> (Integer, Integer)))
--      -> Maybe Integer
--      -> Maybe (Integer -> (Integer, Integer))

-- <*> :: Applicative f => f (c -> d) -> f d -> f d

-- in context: applying the first Maybe to the second,
-- f = Maybe, c = Integer, d = (Integer, Integer)
-- <*> :: Maybe (Integer -> (Integer, Integer))
--     -> Maybe Integer
--     -> Maybe (Integer, Integer)


-- 3.
x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y'


-- 4.
xs :: [Integer]
xs = [1, 2, 3]

ys :: [Integer]
ys = [4, 5, 6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x' <*> y''

-- (,) :: a -> b -> (a, b)
-- sum :: (Foldable t, Num a) => t a -> a

-- (,) <$> x' <*> y :: Maybe (Integer, Integer)
-- fmap :: ((Integer, Integer), Integer) -> Maybe (Integer, Integer) -> Maybe Integer


-- Exercise: Identity instance
-- Write an Applicative instance for Identity

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

-- pure :: a -> f a
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b


-- Exercise: Constant Instance
-- Write an Applicative instance for Constant

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant q) = Constant q

instance Monoid a => Applicative (Constant a) where
-- pure :: Monoid a => p -> (Constant a) p
-- Constant :: Monoid a => a -> (Constant a) b
-- mempty :: Monoid a => a
  pure _ = Constant mempty

-- <*> :: Monoid e => Constant e (a -> b) -> Constant e a -> Constant e b
  (<*>) (Constant e) (Constant e') = Constant (e <> e')

-- (<*>)           Constant (Sum 1)             Constant (Sum 2)   =   Constant (Sum 3)
-- <*> :: Num n => Constant (Sum n) (a -> b) -> Constant (Sum n) a ->  Constant (Sum n) b


-- Exercise: Fixer Upper
-- Given the function and values provided, use (<$>) from Functor, (<*>) and pure from Applicative to fill in missing bits of the broken code to make it work

-- 1.
q1 :: Maybe String
q1 = const <$> Just "Hello" <*> pure "World"

-- const :: a -> b -> a
-- Just "Hello" :: Maybe String

-- fmap :: Functor f => (x -> y) -> f x -> f y
-- fmap :: (String -> (b -> String)) -> Maybe String -> Maybe (b -> String)

-- "World" :: String
-- ? :: Maybe (b -> String) -> ? String -> ?
-- ? :: String -> Maybe String

-- <*> :: Applicative f => f (a -> b) -> f a -> f b
-- <*> :: Maybe (String -> String) -> Maybe String -> Maybe String

-- pure :: Applicative f => a -> f a
-- pure :: String -> Maybe String


-- 2.
q2 :: (Num a, Num b, Num e) => Maybe (a, b, String, [e])
q2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

-- fmap :: Functor f => (x -> y) -> f x -> f y

-- specialized to (,,,) and Just 90
-- f = Maybe, x = Num a => a, y = (b -> c -> d -> (a, b, c, d))
-- fmap :: Num a => (a -> (b -> c -> d -> (a, b, c, d)))
--      -> Maybe a
--      -> Maybe (b -> c -> d -> (a, b, c, d))
-- fmap (,,,) (Just 90) :: Maybe (b -> c -> d -> (a, b, c, d))


-- <*> :: Applicative f => f (x -> y) -> f x -> f y

-- specialized to fmap (,,,) (Just 90) and Just 10
-- f = Maybe, x = Num b => b, y = c -> d -> (a, b, c, d)
-- <*> :: (Num a, Num b) => Maybe (b -> (c -> d -> (a, b, c, d)))
--     -> Maybe b
--     -> Maybe (c -> d -> (a, b, c, d))

-- specialized to some value of type Maybe (c -> d -> (a, b, c, d)) and Just "Tierness"
-- f = Maybe, x = c = String, y = d -> (a, b, c, d)
-- <*> :: (Num a, Num b)
--     => Maybe (String -> (d -> (a, b, String, d)))
--     -> Maybe String
--     -> Maybe (d -> (a, b, String, d))

-- specialized to some value of type Maybe (d -> (a, b, String, d)) and Just [1, 2, 3]
-- f = Maybe, x = Num e => [e],
-- <*> :: Num a, Num b, Num e
--     => Maybe ([e] -> (a, b, String, [e]))
--     -> Maybe [e]
--     -> Maybe (a, b, String, [e])


-- Chapter Exercises

-- Given a type that has an instance of Applicative, specialize the types of the methods. Test your specialization in the REPL. One way to do this is to bind aliases of the typeclass methods to more concrete types that have the type we told you to fill in.

-- 1.
-- Type []

-- Methods
-- pure :: a -> [a]
-- (<*>) :: [a -> b] -> [a] -> [b]


-- 2.
-- Type IO

-- Methods
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b


-- 3.
-- Type (,) a

-- Methods
-- pure :: Monoid a => x -> (a, x)
-- (<*>) :: Monoid a => (a, (x -> y)) -> y) -> (a, x) -> (a, y)


-- 4.
-- Type (->) e

-- Methods
-- pure :: a -> (e -> a)
-- (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)


-- Combinations
-- Remember the vowels and stops exercise in the folds chapter? Write the function to generate the possible combinations of three inputs using liftA3 from Control.Applicative

-- liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
