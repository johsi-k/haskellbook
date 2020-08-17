-- divideThenAdd x y = (x / y) + 1
-- without an explicit type signature, this is interpreted as divideThenAdd :: Fractional a => a -> a -> a

-- now load this with a type that asks only for a Num instance
-- divideThenAdd :: Num a => a -> a -> a
-- divideThenAdd x y = (x / y) + 1

-- this is fine
subtractThenAdd :: Num a => a -> a -> a
subtractThenAdd x y = (x - y) + 1

-- as is this
divideThenAdd :: Fractional a => a -> a -> a
divideThenAdd x y = (x / y) + 1
-- (Num a, Fractional a) is not necessary because
-- Fractional inherits the Num typeclass
-- instances of the Fractional class can use all functions defined in Num
