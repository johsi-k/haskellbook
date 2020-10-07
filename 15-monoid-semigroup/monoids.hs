import Data.Monoid

-- A monoid is a binary associative operation with an identity.
-- monoid: a typeclass
-- binary associative operation: a function of two args that satisfies the law of associativity
-- identity: a value, which when combined with another, will always return that other value


-- mappend is a binary operator (++) from the Monoid typeclass that joins two lists
-- λ> mappend [1, 2, 3] [4, 5, 6]
-- [1,2,3,4,5,6]

-- the empty list is the identity value
-- λ> mappend [1..5] []
-- [1,2,3,4,5]

-- more generally, using mempty from the Monoid typeclass
-- mappend x mempty = x
-- mappend mempty x = x

-- a monoid follows two laws: associativity and identity

-- class Monoid m where
--   mempty  :: m
--   mappend :: m -> m -> m
--   mconcat :: [m] -> m
--   mconcat = foldr mappend mempty

-- mappend: how any 2 values that inhabit a type can be joined together
-- mempty: identity value for the mappend operation


-- Examples of using Monoid

-- List has an instance of Monoid

-- λ> mappend [1, 2, 3] [4, 5, 6]
-- [1,2,3,4,5,6]

-- λ> mconcat [[1..3], [4..6]]
-- [1,2,3,4,5,6]

-- λ> mappend "Trout" " goes well with garlic"
-- "Trout goes well with garlic"

-- λ> (++) [1, 2, 3] [4, 5, 6]
-- [1,2,3,4,5,6]

-- λ> (++) "Trout" " goes well with garlic"
-- "Trout goes well with garlic"

-- λ> foldr (++) [] [[1..3], [4..6]]
-- [1,2,3,4,5,6]

-- λ> foldr mappend mempty [[1..3], [4..6]]
-- [1,2,3,4,5,6]

-- Monoid instance for lists:
-- instance Monoid [a] where
--   mempty  = []

-- instance Semigroup [a] where
--   (<>) = (++)


-- Integers (and all numeric types) don't have a Monoid instance
-- because it isn't clear if numbers should be added or multiplied as a mappend operation
-- both summation and multiplication are monoidal (binary, associative, have an identity value)
-- but each type should only have one unique instance for a given typeclass rather than two (one instance for sum, one instance for product)

-- this won't work
-- λ> x = 1 :: Integer
-- λ> y = 3 :: Integer
-- λ> mappend x y

-- To resolve this, we have the Sum and Product newtypes to wrap numeric values and signal which Monoid instance we want.

-- λ> mappend (Sum 1) (Sum 5)
-- Sum {getSum = 6}

-- λ> mappend (Product 5) (Product 5)
-- Product {getProduct = 25}

-- λ> mappend (Sum 4.5) (Sum 4.5)
-- Sum {getSum = 9.0}


-- More on Sum and Product
-- λ> :i Sum
-- newtype Sum a = Sum {getSum :: a}
-- instance Num a => Monoid (Sum a)

-- λ> :i Product
-- newtype Product a = Product {getProduct :: a}
-- instance Num a => Monoid (Product a)

-- We can use Sum or Product values as a Monoid as long as they contain numeric values
-- λ> :t (<>)
-- (<>) :: Semigroup a => a -> a -> a

-- λ> (Sum 8) <> (Sum 9)
-- Sum {getSum = 17}

-- λ> mappend mempty Sum 9
-- Sum {getSum = 9}

-- To join more than 2 values:
-- λ> mappend (Sum 1) (mappend (Sum 2) (Sum 3))
-- Sum {getSum = 6}

-- λ> Sum 1 <> Sum 1 <> Sum 1
-- Sum {getSum = 3}

-- λ> mconcat [Sum 8, Sum 9, Sum 10]
-- Sum {getSum = 27}

-- We can also unwrap the return values with record field accessors
-- λ> getSum $ mappend (Sum 1) (Sum 1)
-- 2

-- λ> getProduct $ mappend (Product 5) (Product 5)
-- 25

-- λ> getSum $ mconcat [Sum 5, Sum 6, Sum 7]
-- 18


-- Monoids are strongly associated with catamorphisms
-- λ> foldr mappend mempty ([2, 4, 6] :: [Product Int])
-- Product {getProduct = 48}

-- λ> foldr mappend mempty ([2, 4, 6] :: [Sum Int])
-- Sum {getSum = 12}

-- λ> foldr mappend mempty ["blah", "woot"]
-- "blahwoot"


-- Monoid instances must abide by the following laws
-- left identity
-- mappend mempty x = x

-- e.g.
-- λ> mappend mempty (Sum 1)
-- Sum {getSum = 1}

-- λ> mappend mempty [1, 2, 3]
-- [1,2,3]


-- right identity
-- mappend x mempty = x

-- e.g.
-- λ> mappend (Sum 1) mempty
-- Sum {getSum = 1}

-- λ> mappend [1, 2, 3] mempty
-- [1,2,3]


-- associativity
-- mappend x (mappend y z) = mappend (mappend x y) z

-- e.g.
-- λ> (Sum 1) <> (Sum 2 <> Sum 3)
-- Sum {getSum = 6}
-- λ> (Sum 1 <> Sum 2) <> (Sum 3)
-- Sum {getSum = 6}

-- λ> [1] <> ([2] <> [3])
-- [1,2,3]
-- λ> ([1] <> [2]) <> [3]
-- [1,2,3]


-- concatenation
-- mconcat = foldr mappend mempty

-- e.g.
-- λ> mconcat [Sum 1, Sum 2, Sum 3]
-- Sum {getSum = 6}

-- λ> foldr mappend mempty [Sum 1, Sum 2, Sum 3]
-- Sum {getSum = 6}


-- λ> mconcat [[1], [2], [3]]
-- [1,2,3]

-- λ> foldr mappend mempty [[1], [2], [3]]
-- [1,2,3]

-- λ> concat [[1], [2], [3]]
-- [1,2,3]


-- Boolean values have 2 possible monoids - one of conjunction and one of disjunction

-- All represents boolean conjunction: it returns True if and only if all its values it is "appending" are True.

-- λ> All True <> All True
-- All {getAll = True}

-- λ> All True <> All False
-- All {getAll = False}

-- λ> All False <> All True
-- All {getAll = False}

-- λ> All False <> All False
-- All {getAll = False}


-- Any represents boolean disjunction: it returns True if any value is True.

-- λ> Any True <> Any False
-- Any {getAny = True}

-- λ> Any False <> Any True
-- Any {getAny = True}

-- λ> Any False <> Any False
-- Any {getAny = False}

-- λ> Any True <> Any True
-- Any {getAny = True}


-- The Maybe type has more than 2 possible Monoids
-- First and Last: like boolean disjunction, but with explicit reference for the leftmost/rightmost success in a series of Maybe values
-- Unlike with Bool where all we know is True/False, Maybe requires a decision on WHICH Just value to return if there are multiple successes.

-- First returns the first/leftmost non-Nothing value

-- λ> First (Just 1) `mappend` First (Just 2)
-- First {getFirst = Just 1}

-- Last returns the last/rightmost non-Nothing value

-- λ> Last (Just 1) <> Last (Just 2)
-- Last {getLast = Just 2}

-- Both will return something as long as there is at least one Just

-- λ> Last Nothing <> Last (Just 2)
-- Last {getLast = Just 2}

-- λ> First Nothing <> First (Just 2)
-- First {getFirst = Just 2}

-- λ> First Nothing <> First Nothing
-- First {getFirst = Nothing}

-- λ> Last Nothing <> Last Nothing
-- Last {getLast = Nothing}


-- Reusing algebras by asking for algebras
-- There are more possible Monoids for Maybe than First and Last
-- We'll now write that other instance which chooses not one value out of a set of values but combines a values contained within the Maybe a type

-- instance Monoid b => Monoid (a -> b)
-- instance (Monoid a, Monoid b) => Monoid (a, b)
-- instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c)

-- Each of these Monoid instances give a new Monoid for a larger type by reusing the Monoid instances of types that represent components of the larger type
-- This happens even when not all possible values of the larger type contain the value of the type argument, e.g. Just a vs Nothing

-- type arg that doesn't appear in the terms
data Booly a =
    False'
  | True'
  deriving (Eq, Show)

-- no Monoid constraint is needed for a because we're never mappending a values (none exist) nor asking for a mempty of type a.

-- instance Monoid (Booly a) where
  -- mappend False' _ = False'
  -- mappend _ False' = False'
  -- mappend True' True' = True'

instance Semigroup (Booly a) where
  False' <> _ = False'
  _ <> False' = False'
  True' <> True' = True'
