-- To express a list of errors we can make separate checking functions and combine the results, instead of validating all data for a Person at once

-- first we'll add a type alias
type ValidatePerson a = Either [PersonInvalid] a
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)

-- we'll check if age is a positive Integer value
ageOkay :: Age
        -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
  True -> Right age
  False -> Left [AgeTooLow]

nameOkay :: Name
         -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
  True -> Right name
  False -> Left [NameEmpty]

-- Name will only return an invalid result when it's an empty String
-- λ> nameOkay ""
-- Left [NameEmpty]
-- since Name is only a String value, it can be any String, including a string of integers
-- passing a Integer to nameOkay or a String to ageOkay will incur a type error rather than a Left result
-- returning a list of PersonInvalid results will allow us to return both NameEmpty and AgeTooLow when both of those are true

mkPerson :: Name
         -> Age
         -> ValidatePerson Person -- Either [PersonInvalid] Person

mkPerson name age =
  mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name
          -> ValidatePerson Age
          -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge

-- λ> mkPerson "" (-1)
-- Left [NameEmpty,AgeTooLow]
