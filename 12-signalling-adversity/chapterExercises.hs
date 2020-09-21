import Data.List

-- Determine the kinds

-- 1. Given
-- id :: a -> a
-- What is the kind of a?
-- a :: *

-- 2.
-- r :: a -> f a
-- What are the kinds of a and f?
-- a :: *
-- f :: * -> *

-- String processing
-- 1. Write a recursive function named replaceThe which takes a text/string, breaks it into words and replaces each instance of "the" with "a". It's intended only to replace exactly the word "the". notThe is a suggested helper function for accomplishing this.

-- λ> notThe "the"
-- Nothing
-- λ> notThe "blahtheblah"
-- Just "blahtheblah"
-- λ> notThe "woot"
-- Just "woot"

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe str = Just str

-- λ> replaceThe "the cow loves us"
-- "a cow loves us"

-- replaceThe :: String -> String
-- replaceThe = unwords . go . map notThe . words
--   where
--     go [] = []
--     go (Nothing : ws) = "a" : go ws
--     go (Just w : ws) = w : go ws

replaceThe :: String -> String
replaceThe = unwords . map (f . notThe) . words
  where f Nothing  = "a"
        f (Just w) = w


-- 2. Write a recursive function that takes a text/string, breaks it into words, and counts the number of instances of "the" followed by a vowel-initial word.

beginsWithVowel :: String -> Bool
beginsWithVowel "" = False
beginsWithVowel (c:_) = c `elem` vowels

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . words
  where
    go [] = 0
    go ("the":w:ws)
      | beginsWithVowel w = 1 + go ws
      | otherwise = go ws
    go (_:ws) = go ws


-- 3. Return the number of letters that are vowels in a word. Hint: it's helpful to break this into steps. Add any helper functions necessary to achieve your objectives.

-- a) Test for vowelhood
isVowel :: Char -> Bool
isVowel = (`elem` vowels)

-- b) Return the vowels of a string
getVowels :: String -> String
getVowels = filter isVowel

-- c) Count the number of elements returned
countVowels :: String -> Int
countVowels = length . getVowels

-- λ> countVowels "the cow"
-- 2
-- λ> countVowels "Mikolajczak"
-- 4


-- Validate the word
-- Use the Maybe type to write a function that counts the number of vowels in a string and the number of consonants. If the number of vowels exceeds the number of consonants, the function returns Nothing.

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels :: String
vowels = "aeiou"

countConsonants :: String -> Int
countConsonants = length . filter (not . isVowel)

mkWord :: String -> Maybe Word'
mkWord word
  | countVowels word > countConsonants word = Nothing
  | otherwise = Just $ Word' word


-- It's only Natural
-- You'll be presented with a datatype to represent the natural numbers. The only values representable with the naturals are whole numbers from zero to infinity. Your task will be to implement functions to convert Naturals to Integers and Integers to Naturals. The conversion from Naturals to Integers won't return Maybe because Integer is a strict superset of Natural. Any Natural can be represented by an Integer, but the same is not true of any Integer. Negative numbers are not valid natural numbers.

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = succ (natToInteger nat)

-- λ> natToInteger Zero
-- 0
-- λ> natToInteger (Succ Zero)
-- 1
-- λ> natToInteger (Succ (Succ Zero))
-- 2

-- attempt 1
-- integerToNat :: Integer -> Maybe Nat
-- integerToNat n
--   | n < 0 = Nothing
--   | n == 0 = Just Zero
--   | otherwise = Just $ iterate Succ Zero !! fromIntegral n

-- attempt 2
integerToNat' :: Integer -> Nat
integerToNat' = go
  where
    go 0 = Zero
    go n = Succ $ go (n-1)

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | otherwise = Just . go $ n
  where
    go 0 = Zero
    go x = Succ $ go (x-1)


-- Small library for Maybe
-- 1. Simple boolean checks for Maybe values.

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

-- λ> isJust (Just 1)
-- True
-- λ> isJust Nothing
-- False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

-- λ> isNothing (Just 1)
-- False
-- λ> isNothing Nothing
-- True


-- 2. The following is the Maybe catamorphism. You can turn a Maybe value into anything else with this.

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just a) = f a
mayybee b _ Nothing = b

-- λ> mayybee 0 (+1) Nothing
-- 0
-- λ> mayybee 0 (+1) Nothing
-- 2


-- 3. In case you just want to provide a fallback value.
-- Try writing it in terms of the maybe catamorphism

fromMaybe :: a -> Maybe a -> a
fromMaybe a = mayybee a id

-- λ> fromMaybe 0 Nothing
-- 0
-- λ> fromMaybe 0 (Just 1)
-- 1


-- 4. Converting between List and Maybe

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

-- λ> listToMaybe [1, 2, 3]
-- Just 1
-- λ> listToMaybe []
-- Nothing

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- λ> maybeToList (Just 1)
-- [1]
-- λ> maybeToList Nothing
-- []


-- 5. For when we want to drop the Nothing values from our list

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes ((Just x):xs) = x : catMaybes xs

-- λ> catMaybes [Just 1, Nothing, Just 2]
-- [1,2]
-- λ> catMaybes (take 3 $ repeat Nothing)
-- []


-- 6. You'll see this called "sequence" later

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr rf (Just [])
  where
    rf Nothing _ = Nothing
    rf (Just _) Nothing = Nothing
    rf (Just x) (Just acc) = Just (x : acc)

-- on why the intitial value has to be Just []:
-- this is similar to implementing and with foldr
and' :: [Bool] -> Bool
and' = foldr (&&) True
-- True must be the starting point or else the function would always return False

-- foldr f z [] = z
-- foldr f z (x:xs) = f x (foldr f z xs)

-- never encounters the default value
-- flipMaybe [Just 1, Nothing, Just 2]
-- = f (Just 1) (foldr f (Just []) [Nothing, Just 2])

-- foldr f (Just []) [Nothing, Just 2]
-- = f Nothing (foldr f (Just []) [Just 2]) -- short-circuits
-- = Nothing

-- f (Just 1) Nothing
-- = Nothing

-- encounters the default value
-- flipMaybe [Just 1]
-- = f (Just 1) (foldr f (Just []) [])
-- = f (Just 1) (Just [])
-- = Just (1 : [])
-- = Just [1]


-- Small library for Either

-- 1. Try to eventually arrive at a solution that uses foldr.

lefts' :: [Either a b] -> [a]
-- lefts' [] = []
-- lefts' ((Left x):xs) = x : lefts' xs
-- lefts' ((Right _):xs) = lefts' xs

lefts' = foldr rf []
  where
    rf (Left x) acc = x : acc
    rf (Right _) acc = acc


-- 2. Same as the last one. Use foldr eventually.

rights' :: [Either a b] -> [b]
-- rights' [] = []
-- rights' ((Right x):xs) = x : rights' xs
-- rights' ((Left _):xs) = rights' xs

rights' = foldr rf []
  where
    rf (Right x) acc = x : acc
    rf (Left _) acc = acc


-- 3.

partitionEithers' :: [Either a b]
                  -> ([a], [b])
partitionEithers' = foldr rf ([], [])
  where
    rf (Left x) (f, s) = (x : f, s)
    rf (Right x) (f, s) = (f, x: s)


-- 4.

eitherMaybe' :: (b -> c)
             -> Either a b
             -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just $ f x


-- 5. This is a general catamorphism for Either values.

either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' f _ (Left x) = f x
either' _ g (Right x) = g x


-- 6. Same as before, but use the either' function you just wrote.

eitherMaybe'' :: (b -> c)
              -> Either a b
              -> Maybe c
-- eitherMaybe'' f = either' (\x -> Nothing) (\x -> Just $ f x)
eitherMaybe'' f = either' (const Nothing) (Just . f)


-- Unfolds

-- iterate is like a limited unfold that never ends
-- λ> :t iterate
-- iterate :: (a -> a) -> a -> [a]

-- because it never ends, we must use take to get a finite list
-- λ> take 10 $ iterate (+1) 0
-- [0,1,2,3,4,5,6,7,8,9]

-- unfoldr is more general
-- λ> :t unfoldr
-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]

-- using unfoldr to do the same thing as iterate
-- λ> take 10 $ unfoldr (\b -> Just (b, b+1)) 0
-- [0,1,2,3,4,5,6,7,8,9]

-- We bother with this for the same reason we abstracted direct recursion into folds, such as with sum, product and concat.

mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = go (n+x) xs

niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0

mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = go (n*x) xs

niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs
  where go :: [a] -> [[a]] -> [a]
        go xs' [] = xs'
        go xs' (x:xs) = go (xs' ++ x) xs


-- Write your own iterate and unfoldr

-- 1. Write the function myIterate using direct recursion. Compare the behaviour with the built-in iterate to gauge correctness.

myIterate :: (a -> a) -> a -> [a]
myIterate f z = z : myIterate f (f z)


-- 2. Write the function myUnfoldr using direct recursion. Compare with the built-in unfoldr to check your implementation.

myUnfoldr :: (b -> Maybe (a, b))
          -> b
          -> [a]
myUnfoldr f z =
  case f z of
    Nothing -> []
    Just (a, b) -> a : myUnfoldr f b


-- 3. Rewrite myIterate into betterIterate using myUnfoldr. A hint - we used unfoldr to produce the same results as iterate earlier. Do this with different functions and see if you can abstract the structure out.

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))


data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)


-- 1. Write unfold for BinaryTree

unfold :: (a -> Maybe (a, b, a))
       -> a
       -> BinaryTree b
unfold f z =
  case f z of
    Nothing -> Leaf
    Just (lefta, b, righta) -> Node (unfold f lefta) b (unfold f righta)


-- 2. Make a tree builder. Using the unfold function you've made for BinaryTree, write the following function:

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f n
  where
    f 0 = Nothing
    f x = Just (x-1, n-x, x-1)

-- treeBuild 0
-- unfold (const Nothing) 0
-- case Nothing
-- Leaf

-- treeBuild 3
-- = unfold (\x -> Just (2, 0 ,2)) 3
-- = case Just (2, 0, 2) -> Node (unfold f 2) 0 (unfold f 2)

-- unfold f 2
-- unfold (\x -> Just (1, 1, 1)) 2
-- case Just (1, 1, 1) -> Node (unfold f 1) 1 (unfold f 1)

-- unfold f 1
-- unfold (\x -> Just (0, 2, 0)) 1
-- case Just (0, 2, 0) -> Node (unfold f 0) 2 (unfold f 0)
-- Node Leaf 2 Node



-- Appendix on flipMaybe (thanks Andreas!)

-- Simulating the and function with flipMaybe
-- where False is Nothing and True is Just ()
b2m :: Bool -> Maybe ()
b2m x = if x then Just () else Nothing

m2b :: Maybe a -> Bool
m2b Nothing = False
m2b (Just _) = True

andViaMaybe0 :: [Bool] -> Bool
andViaMaybe0 = m2b . flipMaybe . map b2m

-- Plugging this in and simplifying, we'll get back the original definition of and

-- 1. expand definition of flipMaybe
andViaMaybe1 :: [Bool] -> Bool
andViaMaybe1 = m2b . foldr rf (Just []) . map b2m
  where
    rf :: Maybe () -> Maybe [()] -> Maybe [()]
    rf Nothing _ = Nothing
    rf (Just _) Nothing = Nothing
    rf (Just x) (Just acc) = Just (x : acc)

-- 2. combine foldr with map: foldr f z . map g == foldr (f . g) z
andViaMaybe2 :: [Bool] -> Bool
andViaMaybe2 = m2b . foldr (rf . b2m) (Just [])
  where
    rf :: Maybe () -> Maybe [()] -> Maybe [()]
    rf Nothing _ = Nothing
    rf (Just _) Nothing = Nothing
    rf (Just x) (Just acc) = Just (x : acc)

-- 3. combine the definition of rf with the definition of b2m
andViaMaybe3 :: [Bool] -> Bool
andViaMaybe3 = m2b . foldr rf (Just [])
  where
    rf :: Bool -> Maybe [()] -> Maybe [()]
    rf False _ = Nothing
    rf True Nothing = Nothing
    rf True (Just acc) = Just (() : acc)

-- 4. replace all lists inside Just with (), since the m2b function will ignore the contents of Maybe
andViaMaybe4 :: [Bool] -> Bool
andViaMaybe4 = m2b . foldr rf (Just ())
  where
    rf :: Bool -> Maybe () -> Maybe ()
    rf False _ = Nothing
    rf True Nothing = Nothing
    rf True (Just ()) = Just ()

-- 5. since Maybe () is equivalent to Bool, replace all Nothings with False and all Just () with Bool without losing any information
andViaMaybe5 :: [Bool] -> Bool
andViaMaybe5 = foldr rf True
  where
    rf :: Bool -> Bool -> Bool
    rf False _ = False
    rf True False = False
    rf True True = True

-- 6. the definition of rf is the same as that of (&&), so replace it
andViaMaybe :: [Bool] -> Bool
andViaMaybe = foldr (&&) True
