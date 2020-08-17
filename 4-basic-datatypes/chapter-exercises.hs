awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs num = if num < 0 then (negate num) else num

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f tup1 tup2 = ((snd tup1, snd tup2), (fst tup1, fst tup2)) 

-- function that adds 1 to the length of a string arg and returns that result
x = (+)
addOne xs = x w 1
  where w = length xs

-- the identity function
myId x = x

-- when fixed, this function will return 1 from the value (1, 2)
first (a, b) = a