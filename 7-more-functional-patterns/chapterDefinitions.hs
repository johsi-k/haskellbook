-- Pattern matching!

-- We can choose to use none, one or both of the values in the product
data Product a b =
  Product a b
  deriving (Eq, Show)

productUnpackOnlyA :: Product a b -> a
productUnpackOnlyA (Product x _) = x

productUnpackOnlyB :: Product a b -> b
productUnpackOnlyB (Product _ y) = y

productUnpack :: Product a b -> (a, b)
productUnpack (Product x y) = (x, y)

-- We can discriminate by the inhabitants of the sum
-- and choose to do different things based on
-- which constructor in the sum they were
data SumOfThree a b c =
    FirstPossible a
  | SecondPossible b
  | ThirdPossible c
  deriving (Eq, Show)

sumToInt :: SumOfThree a b c -> Integer
sumToInt (FirstPossible _) = 0
sumToInt (SecondPossible _) = 1
sumToInt (ThirdPossible _) = 2
-- remember that the type variable needs to be at least as specific as the
-- inferred type of the bounded thing
-- i.e. FirstPossible 3 is not acceptable

-- We can selectively ignore inhabitants of the sum
sumToInt' :: SumOfThree a b c -> Integer
sumToInt' (FirstPossible _) = 0
sumToInt' _ = 1
