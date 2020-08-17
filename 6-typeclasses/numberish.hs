-- Bad example. Do not write like this.

class Numberish a where
  -- functions available to all instances of this typeclass
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a

-- pretend newtype is data for now
newtype Age =
  Age Integer
  deriving (Eq, Show)

-- create an instance of Numberish for Age
instance Numberish Age where
  -- specify the ways that Age will use the functions of Numberish
  fromNumber n = Age n
  toNumber (Age n) = n
  defaultNumber = Age 65

newtype Year =
  Year Integer
  deriving (Eq, Show)

-- create an instance of Numberish for Year
instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n
  defaultNumber = Year 1988

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
  where integerOfA      = toNumber a
        integerOfAPrime = toNumber a'
        summed =
          integerOfA + integerOfAPrime

-- > sumNumberish (Age 10) (Age 10)
-- > 20
-- How does Haskell know how to derive this when
-- the class def of Numberish doesn't define any terms, only types?

-- In this case it knows to use the instance of Numberish for Age
-- because the arguments to sumNumberish were of type Age

-- > print defaultNumber
-- fails because Haskell has no idea what type defaultNumber is other than that
-- it's provided by Numberish's instances
