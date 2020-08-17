-- Does it typecheck?

-- 1.
-- typechecks by making Person derive Show
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2.
-- typechecks by making Mood derive Eq
data Mood = Blah
          | Woot deriving (Show, Eq)

settleDown x = if x == Woot
               then Blah
               else x

-- 3.
-- a) What values are acceptable inputs to settleDown?
-- Values of type Mood

-- b) What will happen if you try to run settleDown 9? Why?
-- Num (actual type) cannot be compared for equality with Mood (expected type):
-- No instance for (Num Mood) arising from the literal '9'

-- c) What will happen if you try to run Blah > Woot? Why?
-- There will be an error since Mood has no instance of Ord:
-- No instance for (Ord Mood) arising from a use of '>'

-- 4.
-- The following typechecks since all arguments correspond to their declared types
type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"


-- Given the following datatype definitions, what can we do?
data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

-- Which of the following will typecheck? Why or why not?
-- 1.
-- phew = Papu "chases" True doesn't typecheck because
-- String ("chases") is not of type Rocks and
-- Bool (True) is not of type Yeah
phew = Papu (Rocks "chases") (Yeah True)

-- 2.
-- typechecks because both args conform to their respective types
truth = Papu (Rocks "chomskydoz")
             (Yeah True)

-- 3.
-- typechecks because Papu has an Eq instance
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- 4.
-- does not typecheck since Papu doesn't have on Ord instance
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'
