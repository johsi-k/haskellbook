module Lib
    ( CountMe
    , Nope
    , PhhhbbtttEither
    , Identity
    , List
    ) where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Prelude hiding (Left, Right)

-- Bad Monads and their denizens

data CountMe a =
  CountMe Integer a
  deriving (Eq, Show)

instance Functor CountMe where
  -- fmap :: (a -> b) -> CountMe a -> CountMe b
  -- fmap f (CountMe i a) = CountMe (i + 1) (f a)
  fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
  --pure :: a -> CountMe a
  -- CountMe :: Integer -> a -> CountMe a

  pure = CountMe 0
  CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)

instance Monad CountMe where
  return = pure

  -- CountMe n a >>= f =
  --   let CountMe _ b = f a
  --   in CountMe (n + 1) b

  -- CountMe _ a >>= f = f a

  CountMe n a >>= f =
    -- destructures result which is a CountMe b type into its components n' and b
    let CountMe n' b = f a
    in CountMe (n + n') b

  -- (>>=) :: CountMe a -> (a -> CountMe b) -> CountMe b
  -- CountMe n a :: CountMe a
  -- f :: (a -> CountMe b)
  -- CountMe (n + n') b :: CountMe b

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq


-- Chapter Exercises

-- 1. Welcome to the Nope Monad, where nothing happens and nobody cares.
data Nope a =
  NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  -- pure :: a -> Nope a
  pure _ = NopeDotJpg

  -- (<*>) :: (a -> b) -> Nope a -> Nope b
  (<*>) _ NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure

  -- (>>=) :: Nope a -> (a -> Nope b) -> Nope b
  (>>=) NopeDotJpg _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

  -- arbitrary :: Gen (Nope a)
  -- NopeDotJpg :: Nope a

instance EqProp (Nope a) where
  (=-=) = eq


-- 2.
data PhhhbbtttEither b a =
    Left a
  | Right b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left a) = Left (f a)
  fmap _ (Right b) = Right b

instance Applicative (PhhhbbtttEither b) where
  -- pure :: a -> PhhhbbtttEither b a
  pure = Left

  -- (<*>) :: PhhhbbtttEither (x -> y) -> PhhhbbtttEither x -> PhhhbbtttEither y
  (<*>) (Right b) _        = Right b
  (<*>) _        (Right b) = Right b
  (<*>) (Left f) (Left a)  = Left (f a)

instance Monad (PhhhbbtttEither b) where
  return = pure

  -- (>==) :: PhhhbbtttEither b x -> (x -> PhhhbbtttEither b y) -> PhhhbbtttEither b y
  (>>=) (Left a) f = f a
  (>>=) (Right b) _ = Right b

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = oneof [ Left <$> arbitrary
                    , Right <$> arbitrary ]

  -- arbitrary :: Gen (PhhhbbtttEither b a)
  -- Left :: a -> PhhhbbtttEither b a
  -- Right :: b -> PhhhbbtttEither b a

  -- fmap :: (a -> PhhhbbtttEither b a) -> Gen a -> Gen (PhhhbbtttEither b a)
  -- fmap :: (b -> PhhhbbtttEither b a) -> Gen b -> Gen (PhhhbbtttEither b a)

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq


-- 3. Write a Monad instance for Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  -- pure :: a -> Identity a
  pure = Identity

  -- (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure

  -- (>>=) :: Identity a -> (a -> Identity b) -> Identity b
  (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

  -- arbitrary :: Gen (Identity a)
  -- Identity :: a -> Identity a
  -- fmap :: (a -> Identity a) -> Gen a -> Gen (Identity a)


-- 4. This one should be easier than the Applicative instance was. Remember to use the Functor that Monad requires, then see where the chips fall.
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  -- pure :: a -> List a
  pure x = Cons x Nil

  (<*>) fs xs = concat' $ fmap (\f -> fmap f xs) fs

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

instance Monad List where
  return = pure

  -- (>>=) :: List a -> (a -> List b) -> List b
  (>>=) Nil _ = Nil
  (>>=) xs f = concat' $ fmap f xs

-- How does ap permute stuff when >>= (which it is defined in terms of) involves fmap?
-- see how the type of (>>=) is specialized

-- Desugaring ap:
ap' :: Monad m => m (a -> b) -> m a -> m b
-- mf :: m (a -> b), mx :: m a
ap' mf mx = mf >>= (\f -> fmap f mx)

-- (>>=) :: m x -> (x -> m y) -> m y
-- (>>=) :: List (a -> b) -> ((a -> b) -> List b) -> List b

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [ return Nil
                    , Cons <$> arbitrary <*> arbitrary ]

instance Eq a => EqProp (List a) where
  (=-=) = eq
