-- Deconstructing values

newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

-- FarmerType is a Sum
data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer
                deriving Show

-- Farmer is a plain product of Name, Acres and FarmerType
data Farmer =
  Farmer Name Acres FarmerType
  deriving Show

-- Now we'll write a function to break down and unpack the data inside our constructors
isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _                        = False
-- DairyFarmer is one value of FarmerType packed inside the Farmer product type
-- we can pull that value out and pattern match on it

-- Alternate formulation with product that uses record syntax
data FarmerRec =
  FarmerRec { name       :: Name
            , acres      :: Acres
            , farmerType :: FarmerType }
            deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
  case farmerType farmer of
    DairyFarmer -> True
    _           -> False

mouse :: FarmerRec
mouse = FarmerRec (Name "muffy") (Acres 10) WheatFarmer

mouse' :: FarmerRec
mouse' = FarmerRec { farmerType = WheatFarmer, name = Name "muffy", acres = Acres 10 }


-- Function type is exponential
data Quantum =
    Yes
  | No
  | Both
  deriving (Eq, Show)

-- arithmetic of sum types

-- data Either a b = Left a | Right b
-- Right :: b -> Either a b

-- 3 + 3
quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

quantSum2 :: Either Quantum Quantum
quantSum2 = Right No

quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both

quantSum4 :: Either Quantum Quantum
quantSum4 = Left Yes

quantSum5 :: Either Quantum Quantum
quantSum5 = Left No

quantSum6 :: Either Quantum Quantum
quantSum6 = Left Both


-- arithmetic of product types
-- 3 * 3
quantProd1 :: (Quantum, Quantum)
quantProd1 = (Yes, Yes)

quantProd2 :: (Quantum, Quantum)
quantProd2 = (Yes, No)

quantProd3 :: (Quantum, Quantum)
quantProd3 = (Yes, Both)

quantProd4 :: (Quantum, Quantum)
quantProd4 = (No, Yes)

quantProd5 :: (Quantum, Quantum)
quantProd5 = (No, No)

quantProd6 :: (Quantum, Quantum)
quantProd6 = (No, Both)

quantProd7 :: (Quantum, Quantum)
quantProd7 = (Both, Yes)

quantProd8 :: (Quantum, Quantum)
quantProd8 = (Both, No)

quantProd9 :: (Quantum, Quantum)
quantProd9 = (Both, Both)

-- arithmetic of function types
-- 3 ^ 3
quantFlip1 :: Quantum -> Quantum
quantFlip1 Yes  = Yes
quantFlip1 No   = Yes
quantFlip1 Both = Yes

quantFlip2 :: Quantum -> Quantum
quantFlip2 Yes  = Yes
quantFlip2 No   = Yes
quantFlip2 Both = No

quantFlip3 :: Quantum -> Quantum
quantFlip3 Yes  = Yes
quantFlip3 No   = Yes
quantFlip3 Both = Both

quantFlip4 :: Quantum -> Quantum
quantFlip4 Yes  = Yes
quantFlip4 No   = No
quantFlip4 Both = Yes

quantFlip5 :: Quantum -> Quantum
quantFlip5 Yes  = Yes
quantFlip5 No   = Both
quantFlip5 Both = Yes

quantFlip6 :: Quantum -> Quantum
quantFlip6 Yes  = No
quantFlip6 No   = Yes
quantFlip6 Both = Yes

quantFlip7 :: Quantum -> Quantum
quantFlip7 Yes  = Both
quantFlip7 No   = Yes
quantFlip7 Both = Yes

quantFlip8 :: Quantum -> Quantum
quantFlip8 Yes  = Both
quantFlip8 No   = Yes
quantFlip8 Both = No

quantFlip9 :: Quantum -> Quantum
quantFlip9 Yes  = Both
quantFlip9 No   = No
quantFlip9 Both = No

quantFlip10 :: Quantum -> Quantum
quantFlip10 Yes  = Both
quantFlip10 No   = No
quantFlip10 Both = Both

-- ...

-- Consider the following function. According to the equality of a -> b and b^a there should be 2^3 implementations of this function. Does this hold? Write it out and prove it for yourself.

-- 0 0 0
-- 0 0 1
-- 0 1 0
-- 0 1 1
-- 1 0 0
-- 1 0 1
-- 1 1 0
-- 1 1 1

convert1 :: Quantum -> Bool
convert1 Yes  = False
convert1 No   = False
convert1 Both = False

convert2 :: Quantum -> Bool
convert2 Yes  = False
convert2 No   = False
convert2 Both = True

convert3 :: Quantum -> Bool
convert3 Yes  = False
convert3 No   = True
convert3 Both = False

convert4 :: Quantum -> Bool
convert4 Yes  = False
convert4 No   = True
convert4 Both = True

convert5 :: Quantum -> Bool
convert5 Yes  = False
convert5 No   = True
convert5 Both = True

convert6 :: Quantum -> Bool
convert6 Yes  = True
convert6 No   = False
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes  = True
convert7 No   = False
convert7 Both = True

convert8 :: Quantum -> Bool
convert8 Yes  = True
convert8 No   = True
convert8 Both = True

-- Exercises: The Quad
-- Determine how many unique inhabitants each type has.
-- 1.
data Quad =
    One
  | Two
  | Three
  | Four
  deriving (Eq, Show)

-- How many different forms can this take?
eQuad :: Either Quad Quad
eQuad = undefined
-- 4 + 4 = 8 forms

-- 2.
prodQuad :: (Quad, Quad)
prodQuad = undefined
-- 4 * 4 = 16 forms

-- 3.
funcQuad :: Quad -> Quad
funcQuad = undefined
-- 4 ^ 4 = 256

-- 4.
prodTBool :: (Bool, Bool, Bool)
prodTBool = undefined
-- 2 * 2 * 2 = 8 forms

-- 5.
gTwo :: Bool -> Bool -> Bool
gTwo = undefined
-- (2 ^ 2) ^ 2 = 2 ^ (2 * 2) = 16 forms

-- 6.
fTwo :: Bool -> Quad -> Quad
fTwo = undefined
-- (4 ^ 4) ^ 2 = 4 ^ (4 * 2) = 65536 forms
