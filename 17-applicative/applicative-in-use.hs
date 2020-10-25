import Data.Char
import Data.Map
import Control.Applicative

-- Applicative in use

-- List Applicative

-- specialising the types:
-- (<*>) :: f (a -> b) -> f a -> f b
-- (<*>) :: [] (a -> b) -> [] a -> [] b

-- pure :: a -> f a
-- pure :: a -> [] a

-- we can also do this with :set XTypeApplications
-- λ> :t (<*>) @[]
-- (<*>) @[] :: [a -> b] -> [a] -> [b]

-- λ> :t pure @[]
-- pure @[] :: a -> [a]


-- Whereas with Functor we were mapping a single function over a plurality of values, with Applicative we are mapping a plurality of functions over a plurality of values

-- λ> [(+1), (*2)] <*> [2, 4]
-- [3,5,4,8]

-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- f ~ []
-- listApply :: [(a -> b)] -> [a] -> [b]
-- listFmap :: (a -> b) -> [a] -> [b]

-- [(+1), (*2)] <*> [2, 4]
-- [ (+1) 2, (+1) 4, (*2) 2, (*2) 4]

-- ((,) <$> [1, 2]) <*> [3, 4]

-- Mapping the tuple data constructor over the first list,
-- fmap :: Functor f => (x -> y) -> f x -> f y
-- (,) :: a -> b -> (a, b)

-- x = a, y = b -> (a, b)
-- fmap :: Functor f => f a -> f (b -> (a, b))

-- f a = [1, 2]
-- fmap :: Num a => [b -> (a, b)]

-- Applying the first list to the second
-- <*> :: Applicative f => f (c -> d) -> f c -> f d

-- f (c -> d) = Num a => [b -> (a, b)]
-- f = List, c = b, d = (a, b)
-- <*> :: Num a => [b] -> [(a, b)]

-- [b] = [3, 4]
-- <*> :: (Num a, Num b) => [(a, b)]

-- Or we could think of it as:

-- fmapping (,) over the first list
-- [(1, ), (2, )] <*> [3, 4]

-- then applying the first list to the second
-- [(1, 3), (1, 4), (2, 3), (2, 4)]


-- We could also derive the same result with liftA2

-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c

-- λ> liftA2 (,) [1, 2] [3, 4]
-- [(1,3),(1,4),(2,3),(2,4)]


-- A few more examples of the same pattern

-- λ> (+) <$> [1, 2] <*> [3, 5]
-- [4,6,5,7]

-- λ> liftA2 (+) [1, 2] [3, 5]
-- [4,6,5,7]

-- λ> max <$> [1, 2] <*> [1, 4]
-- [1,4,2,4]

-- λ> liftA2 max [1, 2] [1, 4]
-- [1,4,2,4]


-- A few more examples with lookup
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

l :: Maybe String
l = Prelude.lookup 3 [(3, "hello")]

c :: String -> String
c [] = []
c (x:xs) = toUpper x : xs

-- λ> l
-- Just "hello"

-- λ> fmap length l
-- Just 5

-- λ> fmap c l
-- Just "Hello"

-- If we're working with Map data structures instead of lists of tuples, we can import Data.Map and use a Map version of lookup and fromList to accomplish the same thing

-- fromList :: Ord k => [(k, a)] -> Map k a

m :: Map Integer String
m = fromList [(3, "hello")]

-- λ> fmap c $ Data.Map.lookup 3 m
-- Just "Hello"

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

f :: (Eq a, Num a) => a -> Maybe String
f x =
  Prelude.lookup x [ (3, "hello")
           , (4, "julie")
           , (5, "kbai") ]

g :: (Eq a, Num a) => a -> Maybe String
g y =
  Prelude.lookup y [ (7, "sup?")
           , (8, "chris")
           , (9, "aloha") ]

h :: (Eq a, Num a, Num b) => a -> Maybe b
h z =
  Prelude.lookup z [(2, 3), (5, 6), (7, 8)]

m' :: (Eq a, Num a, Num b) => a -> Maybe b
m' x =
  Prelude.lookup x [(4, 10), (8, 13), (1, 9001)]

-- λ> f 3
-- Just "hello"

-- λ> g 8
-- Just "chris"

-- λ> (++) <$> f 3 <*> g 7
-- Just "hellosup?"

-- fmapping (++) over f 3,
-- Just ("hello"++)

-- applying the first Maybe to the second,
-- Just "hellosup?"

-- λ> (+) <$> h 5 <*> m 1
-- Just 9007

-- m 6 has no corresponding tuple a-value and so returns Nothing
-- λ> (+) <$> h 5 <*> m 6
-- Nothing


-- Exercises: Lookups
-- see exercises-without-quickcheck.hs


-- Identity
-- the Identity type introduces structure without changing the semantics of what we're doing.

-- Specializing the types
-- type Id = Identity

-- (<*>) :: f (a -> b) -> f a -> f b
-- (<*>) :: Id (a -> b) -> Id a -> Id b

-- pure :: a -> f a
-- pure :: a -> Id a

-- λ> const <$> [1, 2, 3] <*> [9, 9, 9]
-- [1,1,1,2,2,2,3,3,3]

-- λ> const <$> Identity [1, 2, 3] <*> Identity [9, 9, 9]
-- Identity [1,2,3]

-- Having this extra bit of structure lifts the const function, from mapping over lists to mapping over Identity.


-- Exercise: Identity instance
-- see exercises-without-quickcheck.hs


-- Constant
-- Constant is similar to the Identity type in providing structure, but in addition acts like the const function. It throws away a function application.

-- Specializing the types

-- f ~ Constant e
-- type C = Constant

-- (<*>) :: f (a -> b) -> f a -> f b
-- (<*>) :: C e (a -> b) -> C e a -> C e b

-- pure :: a -> f a
-- pure :: a -> C e a

-- Some examples

-- λ> Constant (Sum 1) <*> Constant (Sum 2)
-- Constant (Sum {getSum = 3})

-- λ> Constant undefined <*> Constant (Sum 2)
-- Constant (Sum {getSum = *** Exception: Prelude.undefined

-- λ> pure 1 :: Constant String Int
-- Constant {getConstant = ""}


-- Exercise: Constant Instance
-- see exercises-without-quickcheck.hs


-- Maybe Applicative
-- Here our function is also embedded in a Maybe structure.
-- Therefore when f is Maybe, we're saying the function itself might not exist becasue we're allowing the possibility of the function to be applied being a Nothing case.

-- type M = Maybe

-- (<*>) :: f (a -> b) -> f a -> f b
-- (<*>) :: M (a -> b) -> M a -> M b

-- pure :: a -> f a
-- pure :: a -> M a

-- Using the Maybe Applicative
-- Consider the following where we validate our inputs to create a value of type Maybe Person because our inputs might be invalid

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if length s > maxLen
  then Nothing
  else Just s

newtype Name =
  Name String deriving (Eq, Show)

newtype Address =
  Address String deriving (Eq, Show)

-- fmap :: Functor f => (a -> b) -> f a -> f b

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s
-- fmap :: (String -> Name) -> Maybe String -> Maybe Name

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a
-- fmap :: (String -> Address) -> Maybe String -> Maybe Address

-- Now we'll make a smart constructor for a Person:

data Person =
  Person Name Address
  deriving (Eq, Show)

-- mkPerson :: String -> String -> Maybe Person
-- mkPerson n a =
--   case mkName n of
--     Nothing -> Nothing
--     Just n' -> case mkAddress a of
--                  Nothing -> Nothing
--                  Just a' -> Just $ Person n' a'

-- While we can leverage fmap for the simpler cases of mkName and mkAddress, we can't make that work with mkPerson.

-- e.g.
-- fmap Person (mkName "Babe")
-- fmap :: (Name -> (Address -> Person)) -> Maybe Name -> Maybe (Address -> Person)

-- fmap Person (mkName "Babe") (mkAddress "old macdonald's")
-- fmap Person (mkName "Babe") :: Maybe (Address -> Person)
-- (mkAddress "old macdonald's") :: Maybe Address

-- ? :: Maybe (Address -> Person) -> Maybe Address -> Maybe Person
-- <*> :: Maybe (Address -> Person) -> Maybe Address -> Maybe Person


-- Applicative gives us what we need
addy :: Maybe Address
addy = mkAddress "old macdonald's"

person :: Maybe (Address -> Person)
person = fmap Person (mkName "Babe")

-- λ> person <*> addy
-- Just (Person (Name "Babe") (Address "old macdonald's"))

-- Now we can rewrite mkPerson:
mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  Person <$> mkName n <*> mkAddress a


-- Before we moooove on

data Cow = Cow {
    name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing


-- Validating to get rid of empty strings and negative numbers
cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              Just (Cow nammy agey weighty)

cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' name' age' weight' =
  Cow <$> noEmpty name' <*> noNegative age' <*> noNegative weight'

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- fmap :: (String -> (Int -> Int -> Cow)) -> Maybe String -> Maybe (Int -> Int -> Cow)

-- <*> :: Applicative f => f (a -> b) -> f a -> f b
-- <*> :: Maybe (Int -> (Int -> Cow)) -> Maybe Int -> Maybe (Int -> Cow)
-- <*> :: Maybe (Int -> Cow) -> Maybe Int -> Maybe Cow

-- e.g.
-- cow1 = Cow <$> noEmpty "Bess"
-- cow1 :: Maybe (Int -> Int -> Cow)

-- cow2 = cow1 <*> noNegative 1
-- cow2 :: Maybe (Int -> Cow)

-- cow3 = cow2 <*> noNegative 2
-- cow3 :: Maybe Cow


-- Alternatively,
-- liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-- liftA3 :: (String -> Int -> Int -> Cow)
--        -> Maybe String
--        -> Maybe Int
--        -> Maybe Int
--        -> Maybe Cow

cowFromString'' :: String -> Int -> Int -> Maybe Cow
cowFromString'' name' age' weight' =
  liftA3 Cow (noEmpty name') (noNegative age') (noNegative weight')


-- Exercise: Fixer Upper
-- see exercises-without-quickcheck.hs
