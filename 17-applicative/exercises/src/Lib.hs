module Lib
    ( List
    , ZipList'
    , Validation
    , Pair
    , Two
    , Three
    , Three'
    , Four
    , Four'
    ) where

import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Checkers
import Control.Applicative

-- List Applicative Exercise
-- Implement the list Applicative. Writing a minimally complete Applicative instance calls for writing the definitions of both pure and <*>.

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

  -- fmap :: (a -> b) -> List a -> List b
  -- f :: (a -> b)

  -- Cons x xs :: List a
  -- x :: a, xs :: List a

  -- Cons y ys :: List b
  -- y :: b, ys :: List b


instance Applicative List where
  pure x = Cons x Nil

  -- fmap :: (x -> y) -> List x -> List y
  -- <*> ::  List (u -> v) -> List u -> List v
  -- flatMap :: (a -> List b) -> List a -> List b

  -- mapping over the list of functions,
  -- flatMap :: ((u -> v) -> List u) -> List (u -> v) -> List u
  -- (<*>) fs xs = flatMap (\f -> fmap f xs) fs
  -- expanding: fs <*> xs = concat $ fmap (\f -> fmap (\x -> f x) xs) fs

  -- mapping over the list of values,
  -- have fs :: List (a -> b), xs :: List a, \x :: a
  -- need result :: List b
  -- fmap :: (p -> q) -> List p -> List q

  (<*>) fs xs = flatMap (\x -> fmap (\f -> f x) fs) xs
  -- expanding: fs <*> xs = concat $ fmap (\x -> fmap (\f -> f x) fs) xs


-- Helper functions
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- fmap :: (x -> y) -> List x -> List y
-- concat' :: List (List z) -> List z

-- x = a, y = List b
-- fmap :: (a -> List b) -> List a -> List (List b)
-- z = b
-- concat' :: List (List b) -> List b

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

-- oneof :: [Gen a] -> Gen a
-- genList :: Arbitrary a => Gen (List a)
-- genList =
--   oneof [return Nil,
--          (do x <- arbitrary
--              xs <- arbitrary
--              return (Cons x xs))]

-- using pure instead of return:
-- do
--  x <- foo
--  y <- bar
--  pure $ baz x y

-- is equivalent to
-- baz <$> foo <*> bar

-- Cons :: a -> List a -> List a
-- <$> :: (a -> b) -> Gen a -> Gen b

-- <$> :: (a -> (List a -> List a)) -> Gen a -> Gen (List a -> List a)
-- Cons <$> foo :: Gen (List a -> List a)

-- <*> :: Gen (List a -> List a) -> Gen (List a) -> Gen (List a)
-- Cons <$> foo <*> bar :: Gen (List a)


instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = oneof [return Nil,
                    Cons <$> arbitrary <*> arbitrary]

instance Eq a => EqProp (List a) where
  (=-=) = eq


-- ZipList Applicative Exercise
-- Implement the ZipList Applicative. Use the checkers library to validate your Applicative instance.

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' n (Cons x xs)
  | n < 1 = Nil
  | otherwise = Cons x (take' (n - 1) xs)

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

repeat' :: a -> List a
repeat' x = xs where xs = Cons x xs

instance Applicative ZipList' where
  -- pure :: a -> ZipList' a
  pure x = ZipList' (repeat' x)

  -- <*> :: ZipList' (a -> b) -> ZipList' a -> ZipList' b
  (<*>) (ZipList' Nil) _ = ZipList' Nil
  (<*>) _ (ZipList' Nil) = ZipList' Nil
  (<*>) (ZipList' (Cons f fs)) (ZipList' (Cons x xs))
    = ZipList' (Cons (f x) fsxs)
    where ZipList' fsxs = ZipList' fs <*> ZipList' xs

  -- ZipList' (Cons f fs) :: ZipList' (m -> n)
  -- Cons f fs :: List (m -> n)
  -- f :: (m -> n), fs :: List (m -> n)

  -- ZipList' (Cons x xs) :: ZipList' m
  -- Cons x xs :: List m
  -- x :: m, xs :: List m

  -- can't do fs <*> xs directly
  -- because the List instance of <*> is not structure-preserving

instance (Arbitrary a) => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary


-- Exercise: Variations on Either
-- Validation has the same representation as Either, but it can be different. The Functor will behave the same, but the Applicative will be different.

data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where

  fmap _ (Failure e) = Failure e
  fmap f (Success x) = Success (f x)

  -- fmap :: (p -> q) -> Validation e p -> Validation e q
  -- f :: (p -> q)
  -- Failure :: e -> Validation e a
  -- Failure e :: Validation e p

  -- Success :: a -> Validation e a
  -- Success x :: Validation e p
  -- x :: p

instance Monoid e => Applicative (Validation e) where
  pure = Success
  -- pure x :: Validation e x
  -- x :: a ; x has to correspond to the success type

  -- Failure :: e -> Validation e a, Success :: a -> Validation e a
  -- <*> :: Validation e (x -> y) -> Validation e x -> Validation e y
  (<*>) (Success f) (Success x) = Success (f x)
  (<*>) (Failure e) (Failure e') = Failure (e <> e') -- Failure type e is Monoid
  (<*>) (Failure e) _ = Failure e
  (<*>) _ (Failure e) = Failure e

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [Failure <$> arbitrary,
                     Success <$> arbitrary]

  -- arbitrary :: Gen (Validation e a)

  -- fmap :: (x -> y) -> Gen x -> Gen y
  -- Failure :: (x -> y), Failure :: e -> Validation e a
  -- x ~ e, y ~ Validation e a
  -- fmap :: (e -> Validation e a) -> Gen e -> Gen (Validation e a)

  -- fmap :: (x -> y) -> Gen x -> Gen y
  -- Success :: (x -> y), Success :: a -> Validation e a
  -- x ~ a, y ~ Validation e a
  -- fmap :: (a -> Validation e a) -> Gen a -> Gen (Validation e a)


instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq


-- Write instances for the following datatypes. Use the checkers library to validate the instances.

-- 1.
data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  -- pure :: Applicative f => a -> f a
  pure a = Pair a a

  -- <*> :: Applicative f => f (a -> b) -> f a -> f b
  -- <*> :: Pair (a -> b) -> Pair a -> Pair b
  (<*>) (Pair p q) (Pair r s) = Pair (p r) (q s)

  -- Pair p q :: Pair (a -> b)
  -- p :: (a -> b)
  -- q :: (a -> b)

  -- Pair r s :: Pair a
  -- r :: a, s :: a

  -- Pair (p r) (q s) :: Pair b
  -- (p r) :: b, (q s) :: b

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

  -- arbitrary :: Gen (Pair a)

  -- fmap :: (x -> y) -> Gen x -> Gen y
  -- Pair :: a -> a -> Pair a
  -- x ~ a, y ~ a -> Pair a
  -- fmap :: (a -> (a -> Pair a)) -> Gen a -> Gen (a -> Pair a)

  -- <*> :: Gen (x -> y) -> Gen x -> Gen y
  -- <*> :: Gen (a -> Pair a) -> Gen a -> Gen (Pair a)

instance Eq a => EqProp (Pair a) where
  (=-=) = eq


-- 2.
data Two a b = Two a b deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  -- pure :: Applicative f => a -> f a
  pure x = Two mempty x

  -- <*> :: Applicative f => f (a -> b) -> f a -> f b
  -- <*> :: Two a (x -> y) -> Two a x -> Two a y
  (<*>) (Two m n) (Two o p) = Two (m <> o) (n p)

  -- Two m n :: Two a (x -> y)
  -- m :: a, n :: (x -> y)

  -- Two o p :: Two a x
  -- o :: a, p :: x

  -- Two (m <> o) (n p) :: Two a y
  -- (m <> o) :: a, n p :: y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

  -- arbitrary :: Gen (Two a b)
  -- fmap :: (x -> y) -> Gen x -> Gen y
  -- Two :: a -> b -> Two a b; Two :: (x -> y)
  -- x ~ a, y ~ (b -> Two a b)
  -- fmap :: (a -> (b -> Two a b)) -> Gen a -> Gen (b -> Two a b)

  -- <*> :: Gen (x -> y) -> Gen x -> Gen y
  -- <*> :: Gen (b -> Two a b) -> Gen b -> Gen (Two a b)


instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq


-- 3.
data Three a b c = Three a b c deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  -- pure :: Applicative f => a -> f a
  pure x = Three mempty mempty x

  -- <*> :: Applicative f => f (a -> b) -> f a - f b
  -- <*> :: Three a b (x -> y) -> Three a b x - Three a b y
  (<*>) (Three p q r) (Three s t u) = Three (p <> s) (q <> t) (r u)

  -- Three p q r :: Three a b (x -> y)
  -- p :: a, q :: b, r :: (x -> y)

  -- Three s t u :: Three a b x
  -- s :: a, t :: b, u :: x

  -- Three (p <> s) (q <> t) (r u) :: Three a b y
  -- (p <> s) :: a, (q <> t) :: b, (r u) :: y

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

  -- arbitrary :: Gen (Three a b c)

  -- liftA3 :: (w -> x -> y -> z) -> Gen w -> Gen x -> Gen y -> Gen z
  -- liftA3 :: (a -> b -> c -> Three a b c) -> Gen a -> Gen b -> Gen c -> Gen (Three a b c)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq


-- 4.
data Three' a b = Three' a b b deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

  -- Three' x y z :: Three' a b
  -- x :: a, y :: b, z :: b

instance Monoid a => Applicative (Three' a) where
  -- pure :: Applicative f => a -> f a
  -- pure :: x -> (Three' a) x
  pure x = Three' mempty x x

  -- <*> :: Applicative f => f (a -> b) -> f a - f b
  -- <*> :: Three' a (x -> y) -> Three' a x - Three' a y
  (<*>) (Three' d e f) (Three' g h i) = Three' (d <> g) (e h) (f i)

  -- Three' d e f :: Three' a (x -> y)
  -- d :: a, e :: (x -> y), f :: (x -> y)

  -- Three' g h i :: Three' a x
  -- g :: a, h :: x, i :: x

  -- Three' (d <> g) (e h) (f i) :: Three' a y
  -- (d <> g) :: a, (e h) :: y, (f i) :: y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

  -- arbitrary :: Gen (Three' a b)
  -- liftA3 :: (w -> x -> y -> z) -> Gen w -> Gen x -> Gen y -> Gen z
  -- liftA3 :: (a -> b -> b -> Three' a b) -> Gen a -> Gen b -> Gen b -> Gen (Three' a b)

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq


-- 5.
data Four a b c d = Four a b c d deriving (Show, Eq)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  -- pure :: Applicative f => a -> f a
  -- pure :: x -> (Four a b c) x
  pure x = Four mempty mempty mempty x

  -- <*> :: Applicative f => f (a -> b) -> f a -> f b
  -- <*> :: Four a b c (x -> y) -> Four a b c x -> Four a b c y
  (<*>) (Four p q r s) (Four h j k l) = Four (p <> h) (q <> j) (r <> k) (s l)

  -- Four p q r s :: Four a b c (x -> y)
  -- p :: a, q :: b, r :: c, s :: (x -> y)

  -- Four h j k l :: Four a b c x
  -- h :: a, j :: b, k :: c, l :: x

  -- Four (p <> h) (q <> j) (r <> k) (s l) :: Four a b c y
  -- (p <> h) :: a, (q <> j) :: b, (r <> k) :: c, (s l) :: y

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
       => Arbitrary (Four a b c d) where
  arbitrary = liftA3 Four arbitrary arbitrary arbitrary <*> arbitrary


instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq


-- 6.
data Four' a b = Four' a a a b deriving (Show, Eq)

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y (f z)
  -- Four' w x y z :: Four' a b
  -- w :: a, x :: a, y :: a, z :: b

instance Monoid a => Applicative (Four' a) where
  -- pure :: x -> (Four' a) x
  pure x = Four' mempty mempty mempty x

  -- <*> :: Applicative f => f (a -> b) -> f a -> f b
  -- <*> :: Four' a (x -> y) -> Four' a x -> Four' a y
  (<*>) (Four' d e f g) (Four' l m n o) = Four' (d <> l) (e <> m) (f <> n) (g o)

  -- Four' d e f g :: Four' a (x -> y)
  -- d :: a, e :: a, f :: a, g :: (x -> y)

  -- Four' l m n o :: Four' a x
  -- l :: a, m :: a, n :: a, o :: x

  -- Four' (d <> l) (e <> m) (f <> n) (g o) :: Four' a y
  -- (d <> l) :: a, (e <> m) :: a, (f <> n) :: a, (g o) :: y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  -- arbitrary :: Gen (Four' a b)
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq
