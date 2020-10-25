{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Data.Monoid
import Control.Applicative

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools

instance EqProp Bull where (=-=) = eq

-- https://hackage.haskell.org/package/checkers-0.1/docs/src/Test-QuickCheck-Checkers.html#eq
-- class EqProp a where (=-=) :: a -> a -> Property

-- | For 'Eq' types as 'EqProp' types
-- eq :: Eq a => a -> a -> Property
-- a `eq` a' = property (a == a')


-- Properties to check that the Applicative m satisfies the applicative properties
-- applicative :: forall m a b c.
--                ( Applicative m
--                , Arbitrary a, CoArbitrary a, Arbitrary b, Arbitrary (m a)
--                , Arbitrary (m (b -> c)), Show (m (b -> c))
--                , Arbitrary (m (a -> b)), Show (m (a -> b))
--                , Show a, Show (m a)
--                , EqProp (m a), EqProp (m b), EqProp (m c)
--                ) =>
--                m (a,b,c) -> TestBatch
-- applicative = const ( "applicative"
--                     , [ ("identity"    , property identityP)
--                       , ("composition" , property compositionP)
--                       , ("homomorphism", property homomorphismP)
--                       , ("interchange" , property interchangeP)
--                       , ("functor"     , property functorP)
--                       ]
--                     )
--  where
--    identityP     :: m a -> Property
--    compositionP  :: m (b -> c) -> m (a -> b) -> m a -> Property
--    homomorphismP :: (a -> b) -> a -> Property
--    interchangeP  :: m (a -> b) -> a -> Property
--    functorP      :: (a -> b) -> m a -> Property

--    identityP v        = (pure id <*> v) =-= v
--    compositionP u v w = (pure (.) <*> u <*> v <*> w) =-= (u <*> (v <*> w))
--    homomorphismP f x  = (pure f <*> pure x) =-= (pure (f x) :: m b)
--    interchangeP u y   = (u <*> pure y) =-= (pure ($ y) <*> u)
--    functorP f x       = (fmap f x) =-= (pure f <*> x)


compositionP u v w = within 100 $ (pure (.) <*> u <*> v <*> w) =-= (u <*> (v <*> w))

compositionP' u' v' w = (pure (.) <*> u <*> v <*> w) =-= (u <*> (v <*> w)) where u = fmap applyFun u'; v = fmap applyFun v'


-- ZipList Monoid
-- beware orphan instances
instance Semigroup a => Semigroup (ZipList a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (ZipList a) where
  -- mempty = ZipList [] -- this is zero, not identity
  mempty = pure mempty

-- To see what pure is:
-- instance Applicative ZipList where
--     pure x = ZipList (repeat x)
--     liftA2 f (ZipList xs) (ZipList ys) = ZipList (zipWith f xs ys)

-- Use the within combinator to figure out which tests are failing
mconcatP :: forall a. (Eq a, EqProp a, Monoid a) => [a] -> Property
mconcatP as = within 1000 $ mconcat as =-= foldr mappend mempty as
-- This shows us that [] is the failing case

-- To see why:
-- foldr mappend mempty []
-- = mempty
-- = pure mempty
-- = ZipList (repeat mempty)
-- = ZipList {getZipList = [Sum {getSum = 0}, Sum {getSum = 0} ...

-- foldr mappend mempty [ZipList [Sum 1]]
-- = mappend (ZipList [Sum 1]) $ foldr mappend mempty []
-- = mappend (ZipList [Sum 1]) $ mempty
-- = ZipList [Sum 1]
-- = ZipList {getZipList = [Sum {getSum = 1}]}

-- Variant which skips the check for []
-- | Implication for properties: The resulting property holds if
-- the first argument is 'False' (in which case the test case is discarded),
-- or if the given property holds.
-- (==>) :: Testable prop => Bool -> prop -> Property
-- False ==> _ = property Discard
-- True  ==> p = property p

mconcatP' :: (Eq a, Monoid a) => [a] -> Property
mconcatP' as = as /= []  ==> mconcat as == foldr mappend mempty as


-- Variant which only tests the beginning of the list
newtype ZipList2 a = ZipList2 [a]
  deriving (Eq, Show, Arbitrary, Semigroup, Monoid) via ZipList a

instance EqProp a => EqProp (ZipList2 a) where
  ZipList2 a =-= ZipList2 b = property $ \(Positive n) -> take n a =-= take n b

main :: IO ()
main = do -- putStrLn "Test suite not yet implemented"

-- Monoid demonstration
-- https://hackage.haskell.org/package/checkers-0.5.6/docs/src/Test.QuickCheck.Classes.html#monoid
-- monoid defines a set of properties to check that the Monoid a satisfies the monoid properties.
-- the argument value is ignored and is present only for its type
-- monoid :: forall a. (Monoid a, Show a, Arbitrary a, EqProp a) => a -> TestBatch
  quickBatch (monoid Twoo)


-- Applicative demonstration
-- These won't terminate when checking for the composition property because the applicative instance for list is quadratic wrt the size of the list.
-- compositionP u v w = (pure (.) <*> u <*> v <*> w) =-= (u <*> (v <*> w))
-- since we're using it twice, it's O(n^3)
  -- quickBatch $ applicative [("b", "w", 1 :: Int)]
  -- quickBatch (applicative (undefined :: [(String, String, Int)]))

-- To see why (and induce failure):
  -- checkBatch (stdArgs {maxSuccess = 100, maxSize = 100, maxShrinks = 1000 }) $ applicative [("a","b", (1 :: Int))]
  quickCheckWith (stdArgs {maxShrinks = 0 }) (compositionP :: [() -> ()] -> [() -> ()] -> [()] -> Property)
  verboseCheckWith stdArgs $ \u v w -> within 10000 ((compositionP' :: [Fun () ()] -> [Fun () ()] -> [()] -> Property) u v w)


-- ZipList Monoid
-- This won't terminate because mconcat fails on []
  -- quickBatch (monoid (ZipList [1 :: Sum Int]))

  -- This returns the failure case
  quickCheck (mconcatP @(ZipList (Sum Int)))

  -- This excludes the check for []
  quickCheck (mconcatP' @(ZipList (Sum Int)))

  quickBatch (monoid @(ZipList2 (Sum Int)) undefined)
