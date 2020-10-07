module Lib
    ( Bull
    , First'
    , Trivial
    , Identity
    , Two
    , Three
    , Four
    , BoolConj
    , BoolDisj
    , Or
    , Combine
    , unCombine
    , Comp
    , unComp
    , Validation
    , Mem
    , runMem
    ) where

import Test.QuickCheck hiding (Failure, Success)
import Data.Monoid

-- Testing QuickCheck's patience
-- demonstration of why a Bool Monoid can't have False as the identity, always returning False and still be a valid Monoid

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Semigroup Bull where
  _ <> _ = Fools

instance Monoid Bull where
  mempty = Fools


-- Exercise: Maybe Another Monoid
data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  Nada <> x = x
  x <> Nada = x
  Only a <> Only b = Only (a <> b)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  First' Nada <> x = x
  x <> First' Nada = x
  First' (Only a)  <> _ = First' (Only a)

instance Monoid (First' a) where
  mempty = First' Nada

-- genFirst' :: Arbitrary a => Gen (First' a)
-- genFirst' = do
--   a <- arbitrary
--   frequency [ (1, return $ First' Nada)
--             , (1, return $ First' $ Only a) ]

-- instance Arbitrary a => Arbitrary (First' a) where
--   arbitrary = genFirst'

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary
    = frequency [ (1, return $ First' Nada)
                , (1, (First' . Only) `fmap` arbitrary) ]


-- Chapter exercises

-- 1.
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial


-- 2.
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity b = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where
  -- identity element of the Identity type is the wrapped identity element of the wrapped type
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity `fmap` arbitrary


-- 3.
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen = do
  a <- arbitrary
  b <- arbitrary
  return $ Two a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = twoGen


-- 4.
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return $ Three a b c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = threeGen


-- 5.
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return $ Four a b c d

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = fourGen


-- 6.
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> (BoolConj False) = BoolConj False
  (BoolConj False) <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True
 
-- instance Arbitrary BoolConj where
--   arbitrary = elements [BoolConj True, BoolConj False]

-- instance Arbitrary BoolConj where
--   arbitrary =
--     frequency [ (1, return $ BoolConj True)
--               , (1, return $ BoolConj False) ]

-- genBC :: Gen BoolConj
-- genBC = do
--   a <- arbitrary
--   return (BoolConj a)

-- instance Arbitrary BoolConj where
--   arbitrary = genBC

instance Arbitrary BoolConj where
  arbitrary = BoolConj `fmap` arbitrary


-- 7.
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> _ = BoolDisj True
  (BoolDisj False) <> (BoolDisj True) = BoolDisj True
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj `fmap` arbitrary


-- 8.
data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  (Snd b) <> _ = Snd b
  _ <> (Snd b) = Snd b
  _ <> (Fst a) = Fst a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary =
    frequency [ (1, Fst `fmap` arbitrary)
              , (1, Snd `fmap` arbitrary) ]


-- 9.
newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
  show (Combine _) = "Combine"

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (f <> g)

-- instance Semigroup b => Semigroup (a -> b) where
--   f <> g = \x -> f x <> g x

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine mempty

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine `fmap` arbitrary

-- preliminary checks
x :: Combine Integer (Sum Integer)
x = Combine $ \n -> Sum (n + 1)
y :: Combine Integer (Sum Integer)
y = Combine $ \n -> Sum (n - 1)

uncombine1 :: Sum Integer
uncombine1 = unCombine (x <> y) 0

uncombine2 :: Sum Integer
uncombine2 = unCombine (x <> y) 1

uncombine3 :: Sum Integer
uncombine3 = unCombine (x <> x) 1

uncombine4 :: Sum Integer
uncombine4 = unCombine (y <> x) 1


-- 10.
newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Show (Comp a) where
  show (Comp _) = "Comp"

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance Monoid (Comp a) where
  mempty = Comp id

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp `fmap` arbitrary


-- 11.
data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Success b) <> _ = Success b
  (Failure _) <> (Success b) = Success b
  (Failure a) <> (Failure a') = Failure (a <> a')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary =
    frequency [ (1, Failure `fmap` arbitrary)
              , (1, Success `fmap` arbitrary) ]


-- Monoid exercise 8.
newtype Mem s a =
  Mem { runMem :: s -> (a,s) }

instance Show (Mem s a) where
  show (Mem _) = "Mem"

instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem g =
    Mem (\s -> let (a, s') = f s
                   (a', s'') = g s'
               in (a <> a', s''))
    -- Mem f and Mem g represent state transitions state -> (output, state')
    -- where state is piped from one function to another

instance Monoid a => Monoid (Mem s a) where
  -- state passes through unchanged
  -- mempty = Mem (\s -> (mempty, s))
  mempty = Mem ((,) mempty)

instance (Arbitrary a, Arbitrary s, CoArbitrary s) => Arbitrary (Mem s a) where
  arbitrary = Mem `fmap` arbitrary

-- preliminary checks
f' = Mem $ \s -> ("hi", s + 1)
rmzero = runMem mempty 0 :: (String, Int)
rmleft = runMem (f' <> mempty) 0 :: (String, Int)
rmright = runMem (mempty <> f') 0 :: (String, Int)
