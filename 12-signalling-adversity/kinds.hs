-- Kinds are used to describe the types of type constructors.

-- Type constants: types that take no arguments and are already types
-- λ> :k Int
-- Int :: *
-- λ> :k Bool
-- Bool :: *
-- λ> :k Char
-- Char :: *

-- Type constructors (higher-kinded types) are types that take more types as args.
-- Example takes a type argument a which is used with the Woot data constructor
data Example a = Blah | RoofGoats | Woot a

-- λ> :k Example
-- Example :: * -> *
-- It must be applied to one type to become a concrete type represented by one *

-- The 2-tuple takes two arguments, and must be applied to two types to become concrete
-- λ> :k (,)
-- (,) :: * -> * -> *

-- λ> :k (Int, Int)
-- (Int, Int) :: *

-- Both Maybe and Either have type constructors rather than constants
-- λ> :k Maybe
-- Maybe :: * -> *
-- λ> :k Maybe Int
-- Maybe Int :: *

-- λ> :k Either
-- Either :: * -> * -> *
-- λ> :k Either Int
-- Either Int :: * -> *
-- λ> :k Either Int String
-- Either Int String :: *


-- Lifted and unlifted types
-- standard lifted types: kind *; any datatype that can be inhabited by bottom
-- unlifted types: kind #; cannot be inhabited by bottom

-- data Maybe a = Nothing | Just a

-- Kinds need to match up

-- These match:
-- λ> :k Maybe
-- Maybe :: * -> *
-- λ> :k Maybe Int
-- Maybe Int :: *
-- λ> :k Maybe Bool
-- Maybe Bool :: *

-- λ> :k Int
-- Int :: *
-- λ> :k Bool
-- Bool :: *

-- These do not:
-- λ> :k Maybe Maybe
-- Expecting one more argument to ‘Maybe’
-- Expected a type, but ‘Maybe’ has kind ‘* -> *’

-- This fixes it:
-- λ> :k Maybe (Maybe Char)
-- Maybe (Maybe Char) :: *


-- Lists are also of kind * -> *
-- λ> :k []
-- [] :: * -> *
-- λ> :k [] Int
-- [] Int :: *
-- λ> :k [Int]
-- [Int] :: *

data Trivial = Trivial
-- λ> :k Trivial
-- Trivial :: *

data Unary a = Unary a
-- λ> :k Unary
-- Unary :: * -> *

data TwoArgs a b = TwoArgs a b
-- λ> :k TwoArgs
-- TwoArgs :: * -> * -> *

data ThreeArgs a b c = ThreeArgs a b c
-- λ> :k ThreeArgs
-- ThreeArgs :: * -> * -> * -> *
