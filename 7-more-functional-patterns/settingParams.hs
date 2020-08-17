myNum :: Integer
myNum = 1

-- a value with no parameters
myVal = myNum
-- myVal :: Integer

-- now introduce a paramter named f
myValWithParam f = myNum
-- myValWithParam1 ;: p -> Integer
-- the inferred type p is polymorphic because we don't do anything with it
-- it could be anything

-- if we do something with f, the type changes
myValWithParam' f = f + myNum
-- now it knows f has to be of type Integer because
-- we added it to myNum

stillAFunction :: [a] -> [a] -> [a] -> [a]
stillAFunction a b c = a ++ b ++ c

-- still more params!
myValWithMoreParams f g = myNum
-- myValWithMoreParams :: p1 -> p2 -> Integer

myValWithMoreParams' f g h = myNum
-- myValWithMoreParams :: p1 -> p2 -> p3 -> Integer
-- p1, p2 and p3 could be but are not required to be different types
-- the types are different since nothing in the code is preventing them from varying
-- the most polymorphic type that works is inferred
