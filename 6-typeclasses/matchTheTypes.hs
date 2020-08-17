import Data.List

-- 1.
-- a)
i :: Num a => a
i = 1
-- b)
-- type signature cannot be substituted for i :: a
-- since the type variable needs to be at least as specific as the bound variable
-- here the unconstrained type variable is more general than
-- the bound variable of instance Num

-- 2.
-- a)
f :: Float
f = 1.0

-- b)
-- type signature cannot be substituted for f :: Num a => a
-- since the type variable needs to be at least as specific as the bound variable
-- here the type variable of instance Num is more general than/is a subclass
-- of the Fractional bound variable

-- 3.
-- a) see 2a)

-- b)
-- type signature can be substituted for f :: Fractional a => a
-- since the type variable needs to be at least as specific as the bound variable
-- here the hierarchy of the type variable equals that of the bound variable (Fractional)

-- 4.
-- a) see 2a)

-- b)
-- type signature can be substituted for f :: RealFrac a => a
-- since the type variable needs to be at least as specific as the bound variable
-- here the type variable of instance RealFrac is more specific than/is a subclass
-- of the Fractional bound variable

-- 5.
-- a)
freud :: a -> a
freud x = x

-- b)
-- type signature can be substituted for freud :: Ord a => a -> a
-- since the type variable needs to be at least as specific as the bound variable
-- here the type variable of instance Ord is more specific than the fully polymorphic bound variable

-- 6.
-- a)
freud' :: a -> a
freud' x = x

-- b)
-- type signature can be substituted for freud' :: Int -> Int
-- since the bound fully polymorphic type a is more general than the Int type

-- 7.
-- a)
myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX

-- b)
-- type signature cannot be substituted for sigmund :: a -> a
-- a type mismatch will ensue because
-- myX expects a fully polymorphic type a
-- but is instead passed an Int (7a)

-- 8.
-- a)
sigmund' :: Int -> Int
sigmund' x = myX

-- b)
-- type signature cannot be substituted for sigmund' :: Num a => a -> a
-- a type mismatch will ensue because
-- myX expects an instance of Num (from the type signature of sigmund') but
-- is instead passed an Int (7a)

-- 9.
-- a)
jung :: Ord a => [a] -> a
jung xs = head (sort xs)

-- b)
-- type signature can be substituted for jung :: [Int] -> Int
-- because the function's implementation suggests that
-- the bound variable should have a specificity of an instance of Ord or lower
-- and Int is a type belonging to a subclass of Ord

-- 10.
-- a)
young :: [Char] -> Char
young xs = head (sort xs)

-- b)
-- type signature can be substituted for young :: Ord a => [a] -> a
-- because the function's implementation suggests that
-- the bound variable needs only to be as specific as an instance of Ord

-- 11.
-- a)
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

-- b)
-- type signature cannot be substituted for signifier :: Ord a => [a] -> a
-- a type error will ensue
-- mySort expects an argument (xs) of type [Char] but is
-- instead given an instance of Ord
