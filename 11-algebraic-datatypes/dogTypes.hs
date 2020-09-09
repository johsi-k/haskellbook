-- Data constructors and values

-- PugType is a type constant and PugData is a constant value
data PugType = PugData

-- Type variable argument a is a phantom - does not occur as an arg to data constructors
-- HuskyData is a constant value
data HuskyType a = HuskyData

-- DogueDeDordeaux is likewise a type constructor with a single type variable argument (doge)
-- Here doge also occurs in the data constructor
data DogueDeBordeaux doge =
  DogueDeBordeaux doge

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[Int]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

-- 10 cannot be reconciled with the type variable bound to String
-- badDoge :: DogueDeBordeaux String
-- badDoge = DogueDeBordeaux 10

data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)

-- Given the datatypes defined in the above sections,

-- 1. Is Doggies a type constructor or a data constructor?
-- It is a type constructor

-- 2. What is the kind of Doggies?
-- Doggies :: * -> *

-- 3. What is the kind of Doggies String?
-- Doggies String :: *

-- 4. What is the type of Husky 10?
-- Husky 10 :: Num a => Doggies a

-- 5. What is the type of Husky (10 :: Integer)?
-- Husky (10 :: Integer) :: Doggies Integer

-- 6. What is the type of Mastiff "Scooby Doo"?
-- Mastiff "Scooby Doo" :: Doggies [Char]

-- 7. Is DogueDeBordeaux a type constructor or a data constructor?
-- It is both a type constructor and a data constructor

-- 8. What is the type of DogueDeBordeaux?
-- DogueDeBordeaux :: doge -> DogueDeBordeaux doge

-- 9. What is the type of DogueDeBordeaux "doggie!"
-- DogueDeBordeaux "doggie!" :: DogueDeBordeaux [Char]
