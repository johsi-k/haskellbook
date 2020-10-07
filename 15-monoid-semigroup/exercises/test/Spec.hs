import Control.Monad
import Data.Monoid
import Test.QuickCheck
import Lib ( Bull
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
           , runMem)

semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidAssoc :: (Eq m, Monoid m)
            => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m)
                   => m
                   -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m)
                   => m
                   -> Bool
monoidRightIdentity a = (a <> mempty) == a


-- Testing QuickCheck's patience
type BullMappend =
  Bull -> Bull -> Bull -> Bool


-- Exercise: Maybe Another Monoid
firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool


-- Chapter Exercises
-- 1.
type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

type TrivId =
  Trivial -> Bool


-- 2.
type IdentityAssoc a =
  Identity a -> Identity a -> Identity a -> Bool

type IdentityId a =
  Identity a -> Bool


-- 3.
type TwoAssoc a b =
  Two a b -> Two a b -> Two a b -> Bool

type TwoId a b =
  Two a b -> Bool


-- 4.
type ThreeAssoc a b c =
  Three a b c -> Three a b c -> Three a b c -> Bool


-- 5.
type FourAssoc a b c d =
  Four a b c d -> Four a b c d -> Four a b c d -> Bool


-- 6.
type BoolConjAssoc =
  BoolConj -> BoolConj -> BoolConj -> Bool

type BoolConjId =
  BoolConj -> Bool


-- 7.
type BoolDisjAssoc =
  BoolDisj -> BoolDisj -> BoolDisj -> Bool

type BoolDisjId =
  BoolDisj -> Bool


-- 8.
type OrAssoc a b =
  Or a b -> Or a b -> Or a b -> Bool


-- 9.
combineSemigroupAssoc :: (Eq b, Semigroup b)
                      => Combine a b
                      -> Combine a b
                      -> Combine a b
                      -> a
                      -> Bool

combineSemigroupAssoc f g h a =
  unCombine (f <> (g <> h)) a == unCombine ((f <> g) <> h) a

type CombineAssoc a b =
  Combine a b -> Combine a b -> Combine a b -> a -> Bool

combMonoidLeftId :: (Eq b, Monoid b)
                 => Combine a b
                 -> a
                 -> Bool

combMonoidLeftId f a =
  mempty <> unCombine f a == unCombine f a

combMonoidRightId :: (Eq b, Monoid b)
                  => Combine a b
                  -> a
                  -> Bool

combMonoidRightId f a =
  unCombine f a <> mempty == unCombine f a

type CombineId a b =
  Combine a b -> a -> Bool


-- 10.
compSemigroupAssoc :: (Eq a, Semigroup a)
                   => Comp a
                   -> Comp a
                   -> Comp a
                   -> a
                   -> Bool

compSemigroupAssoc f g h a =
  unComp (f <> (g <> h)) a == unComp ((f <> g) <> h) a

type CompAssoc a =
  Comp a -> Comp a -> Comp a -> a -> Bool

compMonoidLeftId :: (Eq a, Monoid a)
                 => Comp a
                 -> a
                 -> Bool

compMonoidLeftId f a =
  mempty <> unComp f a == unComp f a

compMonoidRightId :: (Eq a, Monoid a)
                 => Comp a
                 -> a
                 -> Bool

compMonoidRightId f a =
  unComp f a <> mempty == unComp f a

type CompId a =
  Comp a -> a -> Bool


-- 11.
type ValAssoc a b =
  Validation a b -> Validation a b -> Validation a b -> Bool


-- Monoid exercise 8.
memSemigroupAssoc :: (Eq s, Eq a, Semigroup a)
                  => Mem s a
                  -> Mem s a
                  -> Mem s a
                  -> s
                  -> Bool
memSemigroupAssoc f g h s =
  runMem (f <> (g <> h)) s == runMem ((f <> g) <> h) s

type MemAssoc s a =
  Mem s a -> Mem s a -> Mem s a -> s -> Bool

memMonoidLeftId :: (Eq s, Eq a, Monoid a)
                 => Mem s a
                 -> s
                 -> Bool

memMonoidLeftId f s =
  runMem (mempty <> f) s == runMem f s

memMonoidRightId :: (Eq s, Eq a, Monoid a)
                 => Mem s a
                 -> s
                 -> Bool

memMonoidRightId f s =
  runMem (f <> mempty) s == runMem f s

type MemId s a =
  Mem s a -> s -> Bool


main :: IO ()
main = do
  quickCheck (monoidAssoc :: String -> String -> String -> Bool)
  quickCheck (monoidLeftIdentity :: String -> Bool)
  quickCheck (monoidRightIdentity :: String -> Bool)

  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidLeftIdentity :: Bull -> Bool) -- fails
  quickCheck (monoidRightIdentity :: Bull -> Bool) -- fails

  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc String)
  quickCheck (semigroupAssoc :: TwoAssoc String String)
  quickCheck (semigroupAssoc :: ThreeAssoc String String String)
  quickCheck (semigroupAssoc :: FourAssoc String String String String)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc String String)
  quickCheck (combineSemigroupAssoc :: CombineAssoc Integer (Sum Integer))
  quickCheck (compSemigroupAssoc :: CompAssoc (Sum Integer))
  quickCheck (semigroupAssoc :: ValAssoc String String)

  quickCheck (monoidLeftIdentity :: TrivId)
  quickCheck (monoidRightIdentity :: TrivId)
  quickCheck (monoidLeftIdentity :: IdentityId String)
  quickCheck (monoidRightIdentity :: IdentityId String)
  quickCheck (monoidLeftIdentity :: TwoId String String)
  quickCheck (monoidRightIdentity :: TwoId String String)
  quickCheck (monoidLeftIdentity :: BoolConjId)
  quickCheck (monoidRightIdentity :: BoolConjId)
  quickCheck (monoidLeftIdentity :: BoolDisjId)
  quickCheck (monoidRightIdentity :: BoolDisjId)
  quickCheck (combMonoidLeftId :: CombineId Integer (Sum Integer))
  quickCheck (combMonoidRightId :: CombineId Integer (Sum Integer))
  quickCheck (compMonoidLeftId :: CompId (Sum Integer))
  quickCheck (compMonoidRightId :: CompId (Sum Integer))

  quickCheck (memSemigroupAssoc :: MemAssoc Integer String)
  quickCheck (memMonoidLeftId :: MemId Integer String)
  quickCheck (memMonoidRightId :: MemId Integer String)
