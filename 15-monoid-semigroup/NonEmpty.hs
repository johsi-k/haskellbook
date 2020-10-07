import Data.List.NonEmpty as N

-- from Data.List.NonEmpty
-- data NonEmpty a = a :| [a]
-- :| is an infix data constructor that takes two type arguments; it is a product of a and [a]
-- it guarantees that we always have at least one value of type a, which [a] does not guarantee, as any list might be empty

-- Prefix works
data P =
  Prefix Int String

-- Infix works
data Q =
  Int :!!: String

-- Symbolic data constructors cannot be used as a prefix
-- data R =
--   :!!: Int String

-- Alphanumeric data constructors cannot be used as an infix
-- data S =
--   Int Prefix String

-- As a product of two arguments, NonEmpty could also be written as
newtype NonEmpty a =
  NonEmpty (a, [a])
  deriving (Eq, Ord, Show)

-- We cannot write a Monoid for NonEmpty because it has no identity value by design
-- There is no empty list to serve as an identity

xs = 1 :| [2, 3]
ys = 4 :| [5, 6]

-- λ> xs <> ys
-- 1 :| [2,3,4,5,6]

-- λ> N.head xs
-- 1

-- λ> N.length (xs <> ys)
-- 6
