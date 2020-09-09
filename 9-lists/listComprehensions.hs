-- square of every number from 1 to 10
sq = [x^2 | x <- [1..10]]

-- square of even numbers from 1 to 10
sqEven = [x^2 | x <- [1..10], rem x 2 == 0]


-- multiple generators
-- rightmost generator is exhausted first

-- a list of x to the y power
mulgen = [x^y | x <- [1..5], y <- [2, 3]]
-- > [1^2, 1^3, ... 5^2, 5^3]

mulgen' = [x^y | x <- [1..10], y <- [2, 3], x ^ y < 200]
-- > [1,1,4,8,9,27,16,64,25,125,36,49,64,81,100]

-- multiple generators can turn two lists into a list of tuples
tuplefy = [(x, y) | x <- [1, 2, 3], y <- [6, 7]]
-- > [(1,6),(1,7),(2,6),(2,7),(3,6),(3,7)]

tuplefy' = [(x, y) | x <- [1, 2, 3], y <- ['a', 'b']]
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]

-- using list of squares for another list comprehension
mySqr = [x^2 | x <- [1..10]]
anotherThing = [(x, y) | x <- mySqr, y <- [1..3], x < 4]


-- Exercises: Comprehend Thy Lists
-- figure what you think the output lists will be and verify them in the REPL
evenSquares = [x | x <- mySqr, rem x 2 == 0]
-- > [4, 16, 36, 64, 100]

tupleSquares = [(x, y) | x <- mySqr,
                         y <- mySqr,
                         x < 50, y > 50]
-- > [(1, 64), (1, 81), (1, 100),
--    (4, 64), (4, 81), (4, 100),
--    (9, 64), (9, 81), (9, 100),
--    (16, 64), (16, 81), (16, 100),
--    (25, 64), (25, 81), (25, 100),
--    (36, 64), (36, 81), (36, 100),
--    (49, 64), (49, 81), (49, 100)]

fiveTupleSquares =
  take 5 [(x, y) | x <- mySqr,
                   y <- mySqr,
                   x < 50, y > 50]
-- > [(1, 64), (1, 81), (1, 100), (4, 64), (4, 81)]


-- list comprehension with strings

-- remove all lowercase letters from a string
-- elem :: Eq a => a -> [a] -> Bool
caps = [x | x <- "Three Letter Acronym", elem x ['A'..'Z'] ]

-- acronym generator that accepts different strings as inputs
acro xs = [x | x <- xs, elem x ['A'..'Z']]
-- > acro "Self Contained Underwater Breathing Apparatus"
-- > "SCUBA"

-- vowel extractor
myString xs = [x | x <- xs, elem x "aeiou"]


-- Exercises: Square Cube
-- mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

-- 1. Write an expression that makes tuples of the outputs of mySqr and myCube
tupSqCube = [(x, y) | x <- mySqr, y <- myCube]

-- 2. Alter that expression so that it only uses the x and y values that are < 50
tupSqCubeL50 = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- 3. Apply another function to that list comprehension
-- to determine how many tuples inhabit your output list
howManyTups = length tupSqCubeL50
