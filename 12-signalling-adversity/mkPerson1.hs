module EqCaseGuard where

-- data Maybe a = Nothing | Just a
-- Maybe lets us return Nothing when there are no sensible values to return for our intended type a
ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n =
  if even n then Just (n+2) else Nothing

-- Smart constructors for datatypes
-- smart constructors construct values of a type only when they meet certain criteria

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

-- what happens when we make a Person with an empty String for a name or negative years old?
-- mkPerson :: Name -> Age -> Maybe Person
-- mkPerson name age
--   | name /= "" && age >= 0 =
--     Just $ Person name age
--   | otherwise = Nothing

-- what if we now want to know if it was the name, age or both that was bad?
-- to express why we didn't get a successful result from our mkPerson constructor we can use Either

-- Some notes on deriving Eq
-- Compiles without Eq
data PersonInvalid = NameEmpty
                   | AgeTooLow

toString :: PersonInvalid -> String
toString NameEmpty = "NameEmpty"
toString AgeTooLow = "AgeTooLow"

instance Show PersonInvalid where
  show = toString

-- This does not work without an Eq instance
blah :: PersonInvalid -> String
blah pie
  | pie == NameEmpty = "NameEmpty"
  | pie == AgeTooLow = "AgeTooLow"
  | otherwise = undefined

-- it's worth considering that if you needed an Eq instance to pattern match, how would you write them?
instance Eq PersonInvalid where
  (==) NameEmpty NameEmpty = True
  (==) NameEmpty AgeTooLow = False
  (==) AgeTooLow AgeTooLow = True
  (==) AgeTooLow NameEmpty = False

-- data Either a b = Left a | Right b

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
-- Left is an invalid person, when name/age is an invalid input
-- Right is a valid person
mkPerson name age
  | name /= "" && age >= 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | otherwise  = Left AgeTooLow

djali = mkPerson "Djali" 5
-- djali :: Either PersonInvalid Person
-- Right (Person "Djali" 5)

noName = mkPerson "" 10
-- Left NameEmpty
negAge = mkPerson "Djali" (-1)
-- Left AgeTooLow

-- when both name and age are wrong, we'll see the result of the first failure case, not both
badPerson = mkPerson "" (-1)
-- Left NameEmpty

