import Data.Char

-- 1. Given the following datatype

data Weekday =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday

-- we can say
-- a) Weekday is a type with five data constructors

-- 2. and with the same datatype definition in mind, what is the type of the following function, f?
-- f Friday = "Miller Time"
-- c) Weekday -> String

-- 3. Types defined with the data keyword
-- b) must begin with a capital letter

-- 4. The function g xs = xs !! (length xs - 1)
-- c) delivers the final element of xs

-- Ciphers
-- chr :: Int -> Char
-- ord :: Char -> Int

cycleKey :: String -> String -> [(Char, Char)]
cycleKey [] [] = []
cycleKey [] _ = []
cycleKey _ [] = error "cipher cannot be empty"
cycleKey (' ':xs) ys = (' ', ' ') : cycleKey xs ys
cycleKey (x:xs) (y:ys) = (x, y) : cycleKey xs (ys ++ [y])

charShift :: Char -> Int -> Char
charShift c shift = chr(((ord c + shift) - o) `mod` 26 + o)
  where o = ord 'A'

vigenère :: String -> String -> String
vigenère msg key = map mf (cycleKey msg key)
  where mf (' ', _) = ' '
        mf (m, k)   = charShift m (ord k - ord 'A')

unvigenère :: String -> String -> String
unvigenère msg key = map mf (cycleKey msg key)
  where mf (' ', _) = ' '
        mf (m, k)   = charShift m (negate (ord k - ord 'A'))

testVigenère :: IO ()
testVigenère =
  if vigenère "MEET AT DAWN" "ALLY" == "MPPR AE OYWY"
  then putStrLn "yay"
  else putStrLn "nay"

-- As-patterns are a way to pattern match on part of something and still refer to the entire original value
f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t

-- We can use as-patterns with pattern matching on arbitrary data constructors, including lists
doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs

-- Use as-patterns to implement the following functions
-- 1. This should return True if (and only if) all the values in the first list appear in the second list, though they need not be contiguous.

isSubseqOf :: (Eq a)
           => [a]
           -> [a]
           -> Bool

-- isSubseqOf part whole = [x | x <- whole, x `elem` part] == part

isSubseqOf [] _  = True -- an empty string is always a substring of any string
isSubseqOf _ []  = False
isSubseqOf xxs@(x:xs) (y:ys)
  | x == y    = isSubseqOf xs ys
  -- | otherwise = isSubseqOf [x] ys && isSubseqOf xs yss
  -- don't want to equality check on a y which didn't match previously; that causes sequence to not be preserved
  | otherwise = isSubseqOf xxs ys


-- 2. Split a sentence into words, then tuple each word with the capitalized form of each.
capitalizeWords :: String
                -> [(String, String)]
capitalizeWords sentence = [(word, toUpper l : ls) | word@(l:ls) <- words sentence]

-- Language exercises
-- 1. Write a function that capitalizes a word (should really be 'first letter')
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (c:cs) = toUpper c : cs

-- capitalizes first non-space char
capFirstWord :: String -> String
capFirstWord [] = []
capFirstWord (' ':cs) = capFirstWord cs
capFirstWord (c:cs) = toUpper c : cs

-- 2. Write a function that capitalizes sentences in a paragraph. Recognize when a new sentence has begun by checking for periods. Reuse the capitalizeWord function.

-- attempt 1
splitPara :: String -> [String]
splitPara = foldr rf []
  where
    rf c []     = [[c]]
    rf '.' ss   = "." : ss
    rf c (s:ss) = (c : s) : ss

removeLeadingSpace :: String -> String
removeLeadingSpace = dropWhile (== ' ')

capitalizeParagraph :: String -> String
capitalizeParagraph = unwords . map (capitalizeWord . removeLeadingSpace) . splitPara

-- intercalate' :: [a] -> [[a]] -> [a]
-- intercalate' del = foldr (\l acc -> l ++ del ++ acc) []

-- attempt 2
capPara :: String -> String
capPara ""           = ""
capPara ('.':' ':cs) = ". " ++ capPara (capitalizeWord cs)
capPara (c:cs)       = c : capPara cs

-- attempt 3
capSentences :: String -> String
capSentences ss = foldr rf [] (capFirstWord ss)
  where
    rf c ""    = c : ""
    rf '.' str = ". " ++ capFirstWord str
    rf c str   = c : str


-- 1. Create a data structure that captures the phone layout above. The data structure should be able to express enough of how the layout works that you can use it to dictate the behavior of the functions in the following exercises.

type Digit = Char -- any of "1234567890*#"
type Letters = String -- "abc", "+ ", ".,"
type Presses = Int -- valid presses: 1 and up
data DaPhone = DaPhone [(Digit, Letters)] deriving Show

aPhone :: DaPhone
aPhone = DaPhone [
  ('1', "1"),
  ('2', "abc2"),
  ('3', "def3"),
  ('4', "ghi4"),
  ('5', "jkl5"),
  ('6', "mno6"),
  ('7', "pqrs7"),
  ('8', "tuv8"),
  ('9', "wxyz9"),
  ('*', "^*"),
  ('0', "+ 0"),
  ('#', ".,#")
  ]

-- 2. Convert the following conversations into the keypresses required to express them. We're going to suggest types and functions to fill in order to accomplish the goal, but they're not obligatory. If you want to do it differently, go right ahead.

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do you think I am pretty Lol",
   "Lol ya",
   "Just making sure rofl ur turn"]


getPosition :: Char -> String -> Int
getPosition char str = go (toLower char) str 0
  where
    go _ [] _ = 0
    go c (x:xs) count
         | c == x = count + 1
         | otherwise = go c xs (count + 1)

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone
            -> Char
            -> [(Digit, Presses)]
reverseTaps (DaPhone tupList) char =
  foldr rf [] tupList
    where
      rf (digit, letters) acc
        | charPos == 0 = acc
        | isUpper char = ('*', 1) : digitPresses
        | otherwise = digitPresses
        where
          charPos = getPosition char letters
          digitPresses = (digit, charPos) : acc

cellPhonesDead :: DaPhone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead = concatMap . reverseTaps


-- 3. How many times do digits need to be pressed for each message?
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_, p) acc -> p + acc) 0


-- 4. What was the most popular letter for each message? What was its cost? You'll want to combine reverseTaps and fingerTaps to figure out what it cost in taps. reverseTaps is a list because you need to press a different button in order to get capitals.

-- workings for incCharCount
-- aWithA :: String -> String
-- aWithA "" = ""
-- aWithA ('a':cs) = 'A' : aWithA cs
-- aWithA (c:cs) = c : aWithA cs

-- incIntWithA :: [(Char, Int)] -> [(Char, Int)]
-- incIntWithA [] = []
-- incIntWithA (('a', n):ts) = ('a', n + 1) : incIntWithA ts
-- incIntWithA (t:ts) = t : incIntWithA ts

-- reducing functions can be recursive!
-- getCounts :: String -> [(Char, Int)]
-- getCounts = foldr incCharCount []
--   where
--     incCharCount char [] = [(char, 1)]
--     incCharCount char ((c, n):ts)
--       | char == c = (c, n + 1) : ts
--       | otherwise = (c, n) : incCharCount char ts

-- getMax :: [(Char, Int)] -> Maybe (Char, Int)
-- getMax [] = Nothing
-- getMax (t:ts) = Just $ foldr rf t ts
--   where
--     rf (c, n) (ca, na)
--       | n > na = (c, n)
--       | otherwise = (ca, na)

-- mostPopularLetter :: String -> Maybe Char
-- mostPopularLetter str =
--   case getMax . getCounts $ str of
--     Nothing     -> Nothing
--     Just (c, _) -> Just c

-- generalising the 3 functions above
getCounts :: Eq a => [a] -> [(a, Int)]
getCounts = foldr incCharCount []
  where
    incCharCount x [] = [(x, 1)]
    incCharCount x ((x', n):ts)
      | x == x'   = (x', n + 1) : ts
      | otherwise = (x', n) : incCharCount x ts

getMax :: [(a, Int)] -> Maybe (a, Int)
getMax [] = Nothing
getMax (t:ts) = Just $ foldr rf t ts
  where
    rf (x, n) (xa, na)
      | n > na    = (x, n)
      | otherwise = (xa, na)

mostPopularElem :: Eq a => [a] -> Maybe a
mostPopularElem xs =
  case getMax . getCounts $ xs of
    Nothing     -> Nothing
    Just (x, _) -> Just x

mostPopularLetter :: String -> Maybe Char
mostPopularLetter = mostPopularElem

mostPopularCost :: String -> Maybe Presses
mostPopularCost str =
  case getMax . getCounts $ str of
    Nothing     -> Nothing
    Just (c, n) -> Just ((n*) . fingerTaps . reverseTaps aPhone $ c)


-- 5. What was the most popular letter overall? What was the most popular word?
coolestLtr :: [String] -> Maybe Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> Maybe String
coolestWord = mostPopularElem . concatMap words


-- Hutton's Razor
-- 1. Write the "eval" function which reduces an expression to a final sum.
data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add l1 l2) = eval l1 + eval l2

-- 2. Write a printer for the expressions
printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add l1 l2) = printExpr l1 ++ " + " ++ printExpr l2
