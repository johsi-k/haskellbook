-- What's a type and what's data?

-- type constructors -- compile-time
-- -------------------- phase separation
-- data constructors -- runtime

-- when data constructotrs take arguments, those arguments refer to other types

data Price =
  Price Integer
  deriving (Eq, Show)

data Manufacturer =
    Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline =
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle =
    Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)

data Size =
  Size Integer
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 300)

-- Exercises: Vehicles
-- 1. What is the type of myCar?
-- myCar :: Vehicle


-- 2. Given the following, define the functions
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _          = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar


-- 3. Write a function to tell us the manufacturer of a piece of data
getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu
getManu (Plane _ _) = error "planes do not have manufacturers"


-- 4. Given that we're returning the Manufacturer, what will happen if you use this on Plane data?
-- It will throw a runtime exception. We can either expand the output or remove Planes from the allowed inputs.


-- 5. Say you've decided to add the size of the plane as an argument to the Plane constructor. Add that to your datatypes in the appropriate places and change your data and functions appropriately.
