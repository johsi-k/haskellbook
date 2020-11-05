import Data.Monoid

-- t is a higher-kinded type
-- class Foldable (t :: * -> *) where

-- Revenge of the monoids
-- Folding implies a binary associative operation that has an identity value

-- fold combines elements inside a Foldable structure using the Monoid defined for those elements
-- fold :: Monoid m => t m -> m

-- foldMap maps each element of the structure to a Monoid, then combines the results using that instance of Monoid
-- foldMap :: Monoid m => (a -> m) -> t a -> m

-- λ> foldr (+) 0 [1..5]
-- 15

-- The binary associative operation for this fold is (+), so we've specified it without thinking of it as a monoid.

-- fold
-- fold :: Monoid m => t m -> m

sums :: [Sum Integer]
sums = map Sum [1..5]
-- sums = [1, 2, 3, 4, 5]

-- λ> fold sums
-- Sum {getSum = 15}

products :: [Product Integer]
products = [1, 2, 3, 4, 5]

-- λ> fold products
-- Product {getProduct = 120}


-- foldMap
-- foldMap :: Monoid m => (a -> m) -> t a -> m

-- Sum :: a -> Sum a
-- foldMap :: (a -> Sum a) -> [a] -> Sum a
-- λ> foldMap Sum [1, 2, 3, 4]
-- Sum {getSum = 10}

-- Product :: a -> Product a
-- foldMap :: (a -> Product a) -> [a] -> Product a
-- λ> foldMap Product [1, 2, 3, 4]
-- Product {getProduct = 24}

-- All :: Bool -> All
-- foldMap :: (Bool -> All) -> [Bool] -> All
-- λ> foldMap All [True, False, True]
-- All {getAll = False}

-- Any :: Bool -> Any
-- foldMap :: (Bool -> Any) -> [Bool] -> Any
-- λ> foldMap Any [(3 == 4), (9 > 5)]
-- Any {getAny = True}

-- First :: Maybe a -> First a
-- foldMap :: (Maybe a -> First a) -> [Maybe a] -> First a
-- λ> foldMap First [Just 1, Nothing, Just 5]
-- First {getFirst = Just 1}

-- Last :: Maybe a -> Last a
-- foldMap :: (Maybe a -> Last a) -> [Maybe a] -> Last a
-- λ> foldMap Last [Just 1, Nothing, Just 5]
-- Last {getLast = Just 5}

-- The data constructor identifies the Monoid instance (the mappend) for those types
-- However, foldMap can also have a function to map that differs from the Monoid it is using

-- λ> foldMap (*5) products
-- Product {getProduct = 375000}
-- 5 * 10 * 15 * 20 * 25

-- λ> foldMap (*5) sums
-- Sum {getSum = 75}
-- 5 + 10 + 15 + 20 + 25
