-- Data constructor arities
-- nullary
data Example0 =
  Example0
  deriving (Eq, Show)

-- unary
data Example1 =
  Example1 Int
  deriving (Eq, Show)

-- product of Int and String
data Example2 =
  Example2 Int String
  deriving (Eq, Show)

data MyType = MyVal Int deriving (Eq, Show)
-- MyVal :: Int -> MyType

-- Exercises: Cardinality
-- 1. data PugType = PugData
-- 1

-- 2.
-- data Airline =
--     PapuAir
--   | CatapultsR'Us
--   | TakeYourChancesUnited
-- 3

-- 3. Given what we know about Int8, what is the cardinality of Int 16?
-- 32768 + 32767 + 1 = 65536

-- 4. Use the REPL and maxBound and minBound to examine Int and Integer. What can you say about the cardinality of those types?
-- maxBound :: Int = 9223372036854775807
-- minBound :: Int = -9223372036854775808
-- Integer - no instance for Bounded Integer; it is unbounded

-- 5. What's the connection between the 8 in Int8 and that type's cardinality of 256?
-- 2 ^ 8 = 256

-- Exercises: For Example
data Example = MakeExample deriving Show

-- 1. What is the type of data constructor MakeExample? What happens when you request the type of Example?
-- MakeExample :: Example
-- When the type of Example is requested, GHCi complains that the data constructor Example is not in scope.

-- 2. What if you try :info on Example in GHCi? Can you determine what typeclass instances are defined for the Example type using :info in GHCi?
-- Querying :info returns the data declaration and the file, line and column where it is defined.
-- It has an instance of Show.

-- 3. Try making a new datatype like Example but with a single type argument added to MakeExample, such as Int. What has changed when you query MakeExample with :type in GHCi?
data Example' =
  MakeExample' Int deriving Show
-- MakeExample' :: Int -> Example'
