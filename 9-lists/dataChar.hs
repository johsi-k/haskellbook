import Data.Char

-- 1.
-- :t isUpper
-- > isUpper :: Char -> Bool

-- :t toUpper
-- > toUpper :: Char -> Char

-- 2. Write a function that filters all uppercase letters out of a String
filterUpper :: String -> String
filterUpper = filter isUpper

-- 3. Write a function that capitalizes the first letter of a string and returns the entire string
capFirst :: String -> String
capFirst "" = ""
capFirst (x:xs) = toUpper x : xs

-- 4. Make a new version of that function that is recursive such that if you give it the input "woot" it will holler back "WOOT"
cap :: String -> String
cap "" = ""
cap (x:xs) = toUpper x : cap xs

-- 5. Write a function that capitalizes the first letter of a String and returns only that letter as the result
capExtractFirst :: String -> Char
capExtractFirst = toUpper . head

-- 6. Rewrite it as a composed function, and then pointfree
-- (already did)
