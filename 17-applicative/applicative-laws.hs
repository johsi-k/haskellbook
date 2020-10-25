-- Applicative laws

-- 1. Identity
-- pure id <*> v = v

-- λ> pure id <*> [1..5]
-- [1,2,3,4,5]

-- λ> pure id <*> Just "Hello Applicative"
-- Just "Hello Applicative"

-- λ> pure id <*> Nothing
-- Nothing

-- λ> pure id <*> Left "Error'ish"
-- Left "Error'ish"

-- λ> pure id <*> Right 8001
-- Right 8001

-- λ> pure id <*> (+1) $ 2
-- 3

-- pure embeds the id function into some structure so we can use apply intsead of fmap


-- 2. Composition
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

-- pure :: Applicative f => x -> f x
-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- e.g. applied to Maybe,
-- pure (.) :: Maybe ((b -> c) -> (a -> b) -> a -> c)

-- the rest is just applying the rule
-- (.) f g x = f (g x)
-- over the structure

-- λ> pure (.) <*> [(+1)] <*> [(*2)] <*> [1, 2, 3]
-- [3,5,7]
-- λ> [(+1)] <*> ([(*2)] <*> [1, 2, 3])
-- [3,5,7]

-- λ>  pure (.) <*> Just (+1) <*> Just (*2) <*> Just 1
-- Just 3
-- λ> Just (+1) <*> (Just (*2) <*> Just 1)
-- Just 3


-- Homomorphism
-- A homomorphism is a structure-preserving map between 2 algebraic structures.

-- The law
-- pure f <*> pure x = pure (f x)

-- The law can be thought of as having to do with the monoidal part of the applicative: the result should be the result of the function application without doing anything other than combining the structural bits.

-- pure (+1) <*> pure 1 :: Maybe Int
-- = Just (+1) <*> Just 1
-- = Just $ (+1) 1
-- = Just 2

-- pure ((+1) 1) :: Maybe Int
-- = Just 2

-- pure (+1) <*> pure 1 :: Either a Int
-- = Right (+1) <*> Right 1
-- = Right $ (+1) 1
-- = Right 2

-- pure ((+1) 1) :: Either a Int
-- = Right 2

-- 4. Interchange

-- The law
-- u <*> pure y = pure ($ y) <*> u
-- where u is a function embedded in some structure

-- Sectioning $ with y creates an environment where there is a y awaiting a function application
-- Sectioning $ with y applies the second arg only, not the first.
-- ($ 2) = \f -> f $ 2
-- ($)   :: (a -> b) -> a -> b
-- ($ 2) :: (a -> b)      -> b

-- In essence it is
-- f x = ($ x) f
-- lifted into functors

-- Some examples:
-- [(+1), (*2)] <*> pure 1 = pure ($ 1) <*> [(+1), (*2)]

-- <*> :: Applicative f => f (x -> y) -> f x -> f y

-- specializing to [(+1), (*2)] and pure 1
-- (+1) :: Num p => p -> p, (*2) :: Num p => p -> p
-- pure 1 :: Num q => [q]
-- f = [], x = y
-- <*> :: Num a => [(a -> a)] -> [a] -> [a]

-- pure :: Applicative f => x -> f x
-- ($ 1) :: Num a => (a -> b) -> b

-- specializing pure to ($ 1),
-- pure :: (Applicative f, Num a) => ((a -> b) -> b) -> f ((a -> b) -> b)

-- <*> :: Applicative f => f (x -> y) -> f x -> f y

-- specializing to (Applicative f, Num a) => ((a -> b) -> b) and [(+1), (*2)],
-- (+1) :: Num p => p -> p, (*2) :: Num p => p -> p
-- f = [], x = (a -> b), y = b, b = a
-- <*> :: Num a => [(a -> a) -> a] -> [(a -> a)] -> [a]

-- [(+1), (*2)] <*> pure 1 = pure ($ 1) <*> [(+1), (*2)]

-- <*> :: Applicative f => f (a -> b) -> f a -> f b

-- Evaluating,
-- [(+1), (*2)] <*> pure 1
-- = [(+1), (*2)] <*> [1]
-- = [(+1) 1, (*2) 1]
-- = [2, 2]

-- pure ($ 1) <*> [(+1), (*2)]
-- = [($ 1)] <*> [(+1), (*2)]
-- = [($ 1) (+1), ($ 1) (*2)]
-- = [2, 2]
