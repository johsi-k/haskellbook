import Data.Int

-- Sum types
-- Exercises: Pity the Bool
-- 1. Given a datatype
data BigSmall =
    Big Bool
  | Small Bool
  deriving (Eq, Show)
-- What is the cardinality of this datatype?

-- Card(BigSmall)
-- = Card(Big) + Card(Small)
-- = Card(Bool) + Card(Bool)
-- = 2 + 2
-- = 4

-- 2. Given a datatype
-- bring Int8 in scope

data NumberOrBool =
    Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

-- parentheses due to syntactic collision between (-) minus and the negate function
myNumba = Numba (-128)

-- What is the cardinality of NumberOrBool?
-- Card(NumberOrBool)
-- = Card(Numba) + Card(BoolyBool)
-- = Card(Int8) + Card(Bool)
-- = 256 + 2
-- = 258

-- What happens if you try to create a Numba with a numeric literal larger than 127? And with a numeric literal smaller than (-128)?
n = Numba (-128)
-- a -Woverflowed-literals warning arises
-- curiously no spurious warning is given with (-128) like the book anticipates


-- Product Types
data QuantumBool = QuantumTrue
                 | QuantumFalse
                 | QuantumBoth
                 deriving (Eq, Show)

-- MkTwoQs takes two args, making it a product of the two types
data TwoQs =
  MkTwoQs QuantumBool QuantumBool
  deriving (Eq, Show)
-- Card(TwoQs) =
-- = Card(QuantumBool) * Card(QuantumBool)
-- = 3 * 3
-- = 9

-- could also have been written as a type alias with the same cardinality
type TwoQs' = (QuantumBool, QuantumBool)


-- Record syntax
-- Records are product types with additional syntax to provide convenient accessors to fields

-- data Person =
--   MkPerson String Int
--   deriving (Eq, Show)

-- sample data
-- jm = MkPerson "julie" 108
-- ca = MkPerson "chris" 16

-- namae :: Person -> String
-- namae (MkPerson s _) = s

-- is equivalent to:
data Person =
  Person { name :: String
         , age :: Int }
         deriving (Eq, Show)

-- record field accessors are functions that go from the product type to a member of product
-- name :: Person -> String
-- age :: Person -> Int

jm = Person "julie" 108
ca = Person "chris" 16


-- Algebraic rules for products and sums apply in type systems

-- the distributive property
-- a * (b + c) -> (a * b) + (a * c)
-- this applies to Haskell's types too

data Fiction = Fiction' deriving Show
data Nonfiction = Nonfiction' deriving Show

-- note that it is the type constructors which are arguments to FictionBook and NonfictionBook
-- data BookType = FictionBook Fiction
--               | NonfictionBook Nonfiction
--               deriving Show

type AuthorName = String

-- data Author = Author (AuthorName, BookType)
-- not in normal form because BookType remains to be evaluated

-- rewriting in normal form,
data Author' =
    FictionBook AuthorName
  | NonfictionBook AuthorName
  deriving Show


-- Exercises: How Does Your Garden Grow?

-- 1. Given the type
data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardener = String

data Garden =
  Garden Gardener FlowerType
  deriving Show

-- What is the sum of products normal form of Garden?
data Garden' =
    Gardenia' Gardener
  | Daisy' Gardener
  | Rose' Gardener
  | Lilac' Gardener
  deriving Show
