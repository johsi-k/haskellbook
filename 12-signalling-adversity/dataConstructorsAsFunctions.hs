-- Data constructors are functions
data Trivial = Trivial deriving Show

-- Nullary data constructors, which take no args, are not like functions
-- λ> Trivial 1
-- Couldn't match expected type ‘Integer -> t’
-- with actual type ‘Trivial’

-- But data constructors that take arguments do behave like functions
data UnaryC = UnaryC Int deriving Show

-- λ> :t UnaryC
-- UnaryC :: Int -> UnaryC
-- λ> UnaryC 10
-- UnaryC 10
-- λ> :t UnaryC 10
-- UnaryC 10 :: UnaryC

-- Arguments to data constructors are typechecked against the specification in their type
data Unary a = Unary a deriving Show

-- λ> :t Unary
-- Unary :: a -> Unary a
-- λ> :t Unary 10
-- Unary 10 :: Num a => Unary a
-- λ> :t Unary "blah"
-- Unary "blah" :: Unary [Char]

-- You cannot hide POLYMORPHIC types from from your type constructor
-- this is invalid:
-- data Unary = Unary a deriving Show
