{-# LANGUAGE InstanceSigs #-}
import Data.Maybe
import Data.Char

-- Short Exercise: Warming up
cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = cap . rev
-- (.) :: (S -> S)
--     -> (S -> S)
--     -> S -> S

fmapped :: String -> String
fmapped = fmap cap rev
-- fmap :: (S -> S)
--      -> ((->) S) S
--      -> ((->) S) S

-- Now we want to return the results of cap and rev as a tuple
tupled :: String -> (String, String)
tupled = (,) <$> cap <*> rev

tupled' :: String -> (String, String)
tupled' = (,) <$> rev <*> cap

-- fmap :: (S -> S -> (S, S))
--      -> ((->) S) S
--      -> ((->) S) (S -> (S, S))

-- (<*>) :: ((->) S) (S -> (S, S))
--       -> ((->) S) S
--       -> ((->) S) (S, S)

-- There is no special reason such a function needs to be monadic, but let's do that too to get some practice. Do it once using do syntax. Then try writing a new version using >>=.

tupledMDo :: String -> (String, String)
tupledMDo = do
  a <- cap
  b <- rev
  return (a, b)

tupledMDo' :: String -> (String, String)
tupledMDo' = do
  a <- rev
  b <- cap
  return (a, b)

tupledMBind :: String -> (String, String)
tupledMBind = cap >>= (\a -> rev >>= \b -> return (a, b))

tupledMBind' :: String -> (String, String)
tupledMBind' = rev >>= (\a -> cap >>= \b -> return (a, b))

-- m ~ ((->) S), a ~ S, b ~ (S, S)

-- (>>=) ::   m       a  -> (a ->  m        b    ) ->   m        b
-- (>>=) :: (((->) S) S) -> (S -> ((->) S) (S, S)) -> (((->) S) (S, S))

-- \a -> rev >>= \b -> return (a, b) :: S -> S -> (S, S)
-- \a -> (>>=) rev (\b -> return (a, b)) :: S -> (S -> (S, S))

-- inner bind
-- (>>=) :: Monad m => m x -> (x -> m y) -> m y
-- rev :: m x
-- (\b -> return (a, b)) :: (x -> m y)

-- m x ~ (S -> S), m ~ ((->) S), x ~ S, y ~ ?
-- return :: p -> m p, return :: p -> (S -> p)
-- (a, b) :: p, p ~ (ta, S)
-- y ~ (S, S), where ta ~ S as determined by outer (>>=)

-- rev :: (S -> S)
-- (\b -> return (a,b)) :: (S -> (S -> (S, S)))
-- >>= :: (S -> S) -> (S -> (S -> (S, S))) -> (S -> (S, S))
-- (>>=) rev (\b ..) :: S -> (S, S)
-- \a -> ((>>=) rev (\b ..)) :: S -> (S -> (S, S))


-- Exercise: Ask
newtype Reader r a =
  Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id


-- Exercise: Reading comprehension

-- 1. Write liftA2 yourself. Think about it in terms of abstracting out the difference between getDogR and getDogR'.
myLiftA2 :: Applicative f
         => (a -> b -> c)
         -> f a -> f b -> f c
myLiftA2 f ra rb = f <$> ra <*> rb


-- 2. Write the following function. Again, it is simpler than it looks.
asks :: (r -> a) -> Reader r a
asks = Reader


-- 3. Implement the Applicative for Reader.

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) =
    -- Reader $ \r -> f (ra r)
    Reader (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  -- pure a = Reader $ const a
  pure = Reader . const

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)
  -- rab :: r -> (a -> b)
  -- ra :: r -> a
  -- rb :: r -> b


-- Exercise: Reader Monad
-- 1. Implement the Reader Monad

join' :: (r -> (r -> a)) -> (r -> a)
join' rRa r = rRa r r

bind' :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
bind' = flip $ (join' .) . fmap

-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
kleisli' :: (a -> (r -> b)) -> (b -> (r -> c)) -> a -> (r -> c)
kleisli' aRb bRc a r = bRc (aRb a r) r

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r
  -- ra :: r -> a
  -- aRb :: (a -> Reader r b)
  -- aRb (ra r) :: Reader r b
  -- runReader (aRb (ra r)) :: r -> b


-- 2. Rewrite the monadic getDogRM to use your Reader datatype.
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

getDogRM' :: Person -> Dog
-- getDogRM' = dogName >>= (\name -> address >>= \add -> return (Dog name add))
getDogRM' =
  runReader $
    Reader dogName
      >>= (\n -> Reader address
        >>= \a -> Reader (const (Dog n a)))

-- dogName :: Person -> DogName
-- address :: Person -> Address
-- Dog :: DogName -> Address -> Dog

-- (>>=) :: Reader r a
--       -> (a -> Reader r b)
--       -> Reader r b

-- outer bind
-- (>>=) :: Reader Person DogName
--       -> (DogName -> Reader Person Dog)
--       -> Reader Person Dog

-- inner bind
-- (>>=) :: Reader Person Address
--       -> (Address -> Reader Person Dog)
--       -> Reader Person Dog


-- Chapter exercises

-- A warm-up stretch
x :: [Integer]
x = [1, 2, 3]

y :: [Integer]
y = [4, 5, 6]

z :: [Integer]
z = [7, 8, 9]

-- Some functions thta zip the lists together and use lookup to find the value associated with a specified key in our zipped lists.

-- lookup :: Eq a => a -> [(a, b)] -> Maybe

-- zip x and y using 3 as the lookup key
-- zip x y = [(1,4),(2,5),(3,6)]
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
-- zip y z = [(4,7),(5,8),(6,9)]
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- It's also nice to have one that will return Nothing, like this one:
-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
-- zip x z = [(1,7),(2,8),(3,9)]
z' n = lookup n $ zip x z

-- Now we want to add the ability to make a Maybe (,) of values using Applicative.

-- Have x1 make a tuple of xs and ys
x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

-- x2 make a tuple of ys and zs
x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

-- Also write x3 which takes one input and makes a tuple of the results of two applications of z' from above
x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'


-- Some helper functions

-- uncurry to allow us to add the two values that are inside a tuple
-- uncurry :: (a -> b -> c) -> (a, b) -> c

-- summed is uncurry with addition as its first argument
summed :: Num c => (c, c) -> c
summed = uncurry (+)

-- Now a function that lifts a Boolean function over two partially applied functions
bolt :: Integer -> Bool
-- use &&, >3, <8
bolt = (&&) <$> (>3) <*> (<8)


-- Finally, we use fromMaybe in the main exercise
-- fromMaybe :: a -> Maybe a -> a
-- You give it a default value and a Maybe value. If the Maybe value is a Just a, it will return the a value. If the value is Nothing, it returns the default value instead.

-- Next, we'll add one that combines sequenceA and Reader in a somewhat surprising way.

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]


main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]

  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]

  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z

  print $ sequenceA [(>3), (<8), even] 7

  -- 1. Fold the Boolean conjuction operator over the list of results of sequA (applied to some value)
  print $ foldr (&&) True $ sequA 5

  -- 2. Apply sequA to s' - you'll need fromMaybe
  print $ sequA $ fromMaybe 0 s'

  -- 3. Apply bolt to ys - you'll need fromMaybe
  print $ bolt $ fromMaybe 0 ys

-- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
-- sequenceA :: (Integral a, Ord a, Num a) => [(a -> Bool)] -> (a -> [Bool])

-- sequenceA [(>3), (<8), even] 7
-- = traverse id [(>3), (<8), even] 7
-- = List.foldr cons_f (pure []) [(>3), (<8), even] 7
-- = cons_f (>3) (List.foldr cons_f (pure []) [(<8), even]) 7
-- = cons_f (>3) (cons_f (<8) (List.foldr cons_f (pure []) [even])) 7
-- = cons_f (>3) (cons_f (<8) (cons_f even (pure []))) 7

-- = cons_f (>3) (cons_f (<8) (liftA2 (:) (id even) (const []))) 7
-- = cons_f (>3) (cons_f (<8) (\x -> even x : (const []) x)) 7
-- = cons_f (>3) (cons_f (<8) (\x -> even x : [])) 7
-- = cons_f (>3) (cons_f (<8) (\x -> [(even x)])) 7

-- = cons_f (>3) (liftA2 (:) (id (<8)) (\x -> [(even x)])) 7
-- = cons_f (>3) (\y -> ((<8) y) : ((\x -> [(even x)]) y)) 7
-- = cons_f (>3) (\y -> (y<8) : [(even y)]) 7
-- = cons_f (>3) (\y -> [(y<8), (even y)]) 7

-- = liftA2 (:) (id (>3)) (\y -> [(y<8), (even y)]) 7
-- = (\z -> ((>3) z) : ((\y -> [(y<8), (even y)]) z)) 7
-- = (\z -> (z>3) : [(z<8), (even z)]) 7
-- = (\z -> [(z>3), (z<8), (even z)]) 7

-- = (\7 -> [(7>3), (7<8), (even 7)])
-- = [(7>3), (7<8), (even 7)]
-- = [True, True, False]
