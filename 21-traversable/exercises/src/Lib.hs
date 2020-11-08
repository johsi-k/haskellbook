module Lib
    ( Identity
    , Constant
    , Optional
    , List
    , Three
    , Pair
    , Big
    , Bigger
    , S
    , Tree
    ) where

import Test.QuickCheck
import Test.QuickCheck.Checkers

-- Chapter Exercises

-- Traversable instances
-- Write a Traversable instance for the datatype provided, filling in any required superclasses. Use QuickCheck to validate your instances.

-- Identity
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  -- fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  -- foldr :: (a -> b -> b) -> b -> Identity a -> b
  foldr f z (Identity a) = f a z

  -- foldMap :: Monoid m => (a -> m) -> Identity a -> m
  foldMap f (Identity a) = f a

instance Traversable Identity where
  -- traverse :: Applicative f => (a -> f b) -> Identity a -> f (t b)
  traverse f (Identity a) = Identity <$> f a

instance (Arbitrary a) => Arbitrary (Identity a) where
  -- arbitrary :: Gen (Identity a)
  -- Identity :: a -> Identity a
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq


-- Constant
newtype Constant a b =
  Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  -- fmap :: (x -> y) -> Constant a x -> Constant a y
  -- Constant :: a -> Constant a b
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  -- foldr :: (x -> y -> y) -> y -> Constant a x -> y
  foldr _ z (Constant _) = z

  -- foldMap :: Monoid m => (x -> m) -> Constant a x -> m
  foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
  -- traverse :: Applicative f => (x -> f y) -> Constant a x -> f (Constant a y)
  traverse _ (Constant a) = pure (Constant a)

instance Arbitrary a => Arbitrary (Constant a b) where
  -- arbitrary :: Gen (Constant a b)
  -- Constant :: a -> Constant a b
  arbitrary = Constant <$> arbitrary

instance (Eq a, Eq b) => EqProp (Constant a b) where
  (=-=) = eq


-- Maybe
data Optional a =
    Nada
  | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  -- fmap :: (a -> b) -> Optional a -> Optional b
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr _ z Nada = z
  foldr f z (Yep a) = f a z

  -- foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  -- traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  -- arbitrary :: Gen (Optional a)
  -- oneof :: [Gen a] -> Gen a
  -- Yep :: a -> Optional a
  arbitrary = oneof [ return Nada
                    , Yep <$> arbitrary]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq


-- List
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  -- fmap :: (x -> y) -> List x -> List y
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  -- foldr :: (a -> b -> b) -> b -> List a -> b
  foldr _ z Nil = z
  foldr f z (Cons x xs) = f x (foldr f z xs)

  -- foldMap :: Monoid m => (a -> m) -> List a -> m
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  -- traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  -- Cons :: a -> List a -> List a
  -- fmap :: (a -> (List a -> List a)) -> f a -> f (List a -> List a)
  -- (<*>) :: f (List a -> List a) -> f (List a) -> f (List a)
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Arbitrary a => Arbitrary (List a) where
  -- arbitrary :: Gen (List a)
  -- Cons :: a -> List a -> List a
  arbitrary = oneof [ return Nil,
                      Cons <$> arbitrary <*> arbitrary ]

instance Eq a => EqProp (List a) where
  (=-=) = eq


-- Three
data Three a b c =
  Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  -- fmap :: (x -> y) -> Three a b x -> Three a b y
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f z (Three _ _ c) = f c z

  -- foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  -- traverse :: Applicative f => (x -> f y) -> Three a b x -> f (Three a b y)
  -- Three :: a -> b -> c -> Three a b c
  -- fmap :: (y -> Three a b y) -> f y -> f (Three a b y)
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq


-- Pair
data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  -- fmap :: (x -> y) -> f x -> f y
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  -- foldr :: (x -> y -> y) -> y -> Pair a x -> y
  foldr f z (Pair _ b) = f b z

  -- foldMap :: Monoid m => (x -> m) -> (Pair a) x -> m
  foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
  -- traverse :: Applicative f => (x -> f y) -> Pair a x -> f (Pair a y)
  -- Pair :: a -> b -> Pair a b
  traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq


-- Big
-- When you have more than one value of type b, use Monoid and Applicative for the Foldable and Traversable instances respectively
data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
  -- fmap :: (x -> y) -> Big a x -> Big a y
  fmap f (Big a b1 b2) = Big a (f b1) (f b2)

instance Foldable (Big a) where
  -- foldr :: (x -> y -> y) -> y -> Big a x -> y
  foldr f z (Big _ b1 b2) = (f b1 . f b2) z

  -- foldMap :: Monoid m => (x -> m) -> Big a x -> m
  foldMap f (Big _ b1 b2) = f b1 <> f b2

instance Traversable (Big a) where
  -- traverse :: (x -> f y) -> Big a x -> f (Big a y)
  -- Big :: a -> b -> b -> Big a b
  -- f b1 :: f y, f b2 :: f y
  traverse f (Big a b1 b2) = Big a <$> f b1 <*> f b2

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  -- arbitrary :: Gen (Big a b)
  -- Big :: a -> b -> b -> Big a b
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq


-- Bigger
data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  -- fmap :: (x -> y) -> Bigger a x -> Bigger a y
  fmap f (Bigger a b1 b2 b3) = Bigger a (f b1) (f b2) (f b3)

instance Foldable (Bigger a) where
  -- foldr :: (x -> y -> y) -> y -> Bigger a x -> y
  foldr f z (Bigger _ b1 b2 b3) = (f b1 . f b2 . f b3) z

  -- foldMap :: Monoid m => (x -> m) -> Bigger a x -> m
  foldMap f (Bigger _ b1 b2 b3) = f b1 <> f b2 <> f b3

instance Traversable (Bigger a) where
  -- traverse :: Applicative f => (x -> f y) -> Bigger a x -> f (Bigger a y)
  traverse f (Bigger a b1 b2 b3) = Bigger a <$> f b1 <*> f b2 <*> f b3

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  -- arbitrary :: Gen (Bigger a b)
  -- Bigger :: a -> b -> b -> b -> Bigger a b
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq


-- S
data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a )
        => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance ( Applicative n
         , Testable (n Property)
         , Eq a
         , Eq (n a)
         , EqProp a )
        => EqProp (S n a) where
  (=-=) = eq

instance Functor n => Functor (S n) where
  -- fmap :: (a -> b) -> S n a -> S n b
  fmap f (S p x) = S (fmap f p) (f x)
  -- data S n a = S (n a) a
  -- S p x :: S n a
  -- p :: n a, x :: a

instance Foldable n => Foldable (S n) where
  -- foldr :: (a -> b -> b) -> b -> S n a -> b
  foldr f z (S p x) = f x (foldr f z p)
  -- p :: n a, x :: a
  -- foldr specialized to f and p :: (a -> b -> b) -> b -> n a -> b

  -- foldMap :: Monoid m => (a -> m) -> S n a -> m
  foldMap f (S p x) = foldMap f p <> f x
  -- p :: n a, x :: a
  -- foldMap specialized to f and p :: (a -> m) -> n a -> m

instance Traversable n => Traversable (S n) where
  -- traverse :: Applicative f => (a -> f b) -> S n a -> f (S n b)
  traverse f (S p x) = S <$> traverse f p <*> f x
  -- p :: n a, x :: a
  -- traverse specialized to f and p :: (a -> f b) -> n a -> f (n b)


-- Instances for Tree
data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node lt a rt) = Node (fmap f lt) (f a) (fmap f rt)

instance Foldable Tree where
  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Empty = z
  foldr f z (Leaf a) = f a z
  foldr f z (Node lt a rt) = foldr f (f a (foldr f z rt)) lt

  -- foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node lt a rt) = foldMap f lt <> f a <> foldMap f rt

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node lt a rt) = Node <$> traverse f lt <*> f a <*> traverse f rt

instance Arbitrary a => Arbitrary (Tree a) where
  -- arbitrary :: Gen (Tree a)
  -- Node :: Tree a -> a -> Tree a -> Tree a
  -- fmap :: (Tree a -> (a -> Tree a -> Tree a)) -> Gen (Tree a) -> Gen (a -> Tree a -> Tree a)
  arbitrary = oneof [ return Empty
                    , Leaf <$> arbitrary
                    , Node <$> arbitrary <*> arbitrary <*> arbitrary ]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq
