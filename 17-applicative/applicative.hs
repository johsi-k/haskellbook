-- Defining Applicative

-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b


-- Functor vs Applicative
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- Any applicative also has a Functor
-- You can define a Functor in terms of a provided Applicative instance

-- fmap f x = pure f <*> x

-- λ> fmap (+1) [1, 2, 3]
-- [2,3,4]

-- λ> pure (+1) <*> [1, 2, 3]
-- [2,3,4]

-- pure embeds a value of any type in the structure we are working with

-- λ> pure 1 :: [Int]
-- [1]
-- λ> pure 1 :: Maybe Int
-- Just 1

-- The left type is handled differently from the right for the same reason here:
-- fmap (+1) (4, 5) = (4, 6)
-- The left type is part of the structure, which is not transformed by function application.

-- λ> pure 1 :: Either a Int
-- Right 1
-- λ> pure 1 :: ([a], Int)
-- ([],1)


-- Applicative functors are monoidal functors
-- ($) :: (a -> b) -> a -> b
-- (<$>) :: (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b


-- With Applicative, we have a Monoid for our structure and function application for our values

-- mappend :: Monoid a => a -> a -> a

-- mappend :: f             f      f
-- $       ::   (a -> b)      a      b
-- (<*>)   :: f (a -> b) -> f a -> f b


-- Some examples to see what this means

-- (a -> b) enriched with "listness"

-- [(*2), (*3)] <*> [4, 5]
-- = [2*4, 2*5, 3*4, 3*5]
-- = [8, 10, 12, 15]


-- (a -> b) enriched with Maybe

-- λ> Just (*2) <*> Just 2
-- Just 4

-- λ> Just (*2) <*> Nothing
-- Nothing

-- λ> Nothing <*> Just 2
-- Nothing

-- λ> Nothing <*> Nothing
-- Nothing


-- Show me the monoids

-- The Functor instance for the 2-tuple ignores the first value inside the tuple

-- λ> fmap (+1) ("blah", 0)
-- ("blah", 1)

-- But the Applicative for the 2-tuple demonstrates the monoid in Applicative nicely for us

-- instance Monoid a => Applicative ((,) a)
-- instance (Monoid a, Monoid b) => Monoid (a, b)

-- The Applicative instance of the 2-tuple doesn't require a Monoid for the b because we're using function application to produce the b
-- However the first value of the tuple still requires a Monoid because we have 2 values and need to turn that into one value of the same type

-- λ> ("Woo", (+1)) <*> (" Hoo!" ,0)
-- ("Woo Hoo!",1)

-- in context,
-- <*> :: (,) a (x -> y) -> (,) a x -> (,) a y
-- <*> :: (a, x -> y) -> (a, x) -> (a, y)


-- Tuple Monoid and Applicative side by side

-- instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
--   (a, b) <> (a', b') = (a <> a', b <> b')

-- instance (Monoid a, Monoid b) => Monoid (a, b) where
--   mempty = (mempty, mempty)

-- instance Monoid a => Applicative ((,) a) where
--   pure x = (mempty, x)
--   (u, f) <*> (v, x) = (u <> v, f x)


-- Maybe Monoid and Applicative

-- While applicatives are monoidal functors, we need to be careful about making assumptions based on this.
-- Monoid and applicative instances aren't required/guaranteed to have the same monoid of structure, and the functorial part may change the way it behaves.

-- instance Semigroup a => Semigroup (Maybe a) where
--   m <> Nothing = m
--   Nothing <> m = m
--   (Just a) <> (Just a') = Just (a <> a')

-- instance Monoid a => Monoid (Maybe a) where
--   mempty = Nothing

-- instance Applicative Maybe where
--   pure = Just

--   Nothing <*> _ = Nothing
--   _ <*> Nothing = Nothing
--   Just f <*> Just a = Just (f a)
