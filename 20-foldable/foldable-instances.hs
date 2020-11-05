-- Demonstrating Foldable instances

-- Identity
data Identity a = Identity a

-- Only foldr / foldMap is required, but we'll alse write foldl
instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

-- for comparison

-- foldr f z [] = z
-- foldr f z (x:xs) = f x (foldr f z xs)

-- foldl f z [] = z
-- foldl f z (x:xs) = foldl f (f z x) xs

-- λ> foldr (*) 1 (Identity 5)
-- 5
-- λ> foldl (*) 5 (Identity 5)
-- 25
-- λ> foldMap (*5) (Identity 100) :: Product Integer
-- Product {getProduct = 500}


-- Maybe
-- This is more interesting because unlike Identity, we have to account for the Nothing cases.
-- When the Maybe value we're folding is Nothing, we need to be able to return some "zero" value, while doing nothing with the folding function. For foldr and foldl, the zero value is the initial value supplied.

-- λ> foldr (+) 1 Nothing
-- 1
-- λ> foldl (+) 1 Nothing
-- 1

-- for foldMap the Monoid's identity value is its zero:
-- λ> foldMap (+1) Nothing :: Sum Integer
-- Sum {getSum = 0}

-- When the Maybe value is a Just, we apply the folding function to the value and dispose of the structure
-- λ> foldr (+) 1 (Just 3)
-- 4

data Optional a =
    Nada
  | Yep a
  deriving (Eq, Show)

instance Foldable Optional where
-- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr _ z Nada = z
  foldr f z (Yep a) = f a z

-- foldl :: (b -> a -> b) -> b -> t a -> b
  foldl _ z Nada = z
  foldl f z (Yep a) = f z a

-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

-- λ> foldMap (+1) Nada :: Sum Int
-- Sum {getSum = 0}

-- λ> foldMap (+1) Nada :: Product Int
-- Product {getProduct = 1}

-- λ> foldMap (+1) (Just 1) :: Sum Int
-- Sum {getSum = 2}


-- Some basic derived operations

-- List of elements of a structure, from left to right
-- toList :: Foldable t => t a -> [a]

-- λ> toList (Just 1)
-- [1]
-- λ> map toList [Just 1, Just 2, Just 3]
-- [[1],[2],[3]]
-- λ> concatMap toList [Just 1, Just 2, Just 3]
-- [1,2,3]

-- λ> concatMap toList [Just 1, Just 2, Nothing]
-- [1,2]

-- λ> toList (1,2)
-- [2] -- 1 doesn't go into the list for the same reason that fmap doesn't apply a function to the 1
-- instance Foldable ((,) a)


-- Tests whether the structure is empty
-- null :: Foldable t => t a -> Bool

-- null returns True on Left and Nothing values, as it does on empty lists
-- λ> null (Left 3)
-- True
-- λ> null []
-- True
-- λ> null Nothing
-- True
-- λ> null (1, 2)
-- False
-- λ> fmap null [Just 1, Just 2, Nothing]
-- [False,False,True]


-- Returns the size/length of a finite structure as an 'Int'
-- length :: Foldable t => t a -> Int
-- For tuples the first argument is part of the t, not part of the a
-- The same applies for type arguments of datatypes such as Maybe and Either

-- λ> length (1, 2)
-- 1
-- λ> length [(1, 2), (3, 4), (5, 6)]
-- 3
-- λ> fmap length [(1, 2), (3, 4), (5, 6)]
-- [1,1,1]

-- λ> fmap length Just [1, 2, 3]
-- 1

-- specialized to length :: t a -> Int,
-- fmap :: (a -> b) -> f a -> f b
-- fmap :: (t a -> Int) -> f (t a) -> f Int

-- specialized to Just :: x -> Maybe x,
-- f ~ ((->) x), t a ~ Maybe x
-- fmap :: (Maybe x -> Int) -> ((-> x) (Maybe x)) -> ((-> x) Int)
-- fmap :: (Maybe x -> Int) -> (x -> Maybe x) -> (x -> Int)

-- λ> fmap length [Just 1, Just 2, Just 3]
-- [1,1,2]

-- λ> fmap length [Just 1, Just 1, Nothing]
-- [1,1,0]


-- Queries if element occurs in the structure
-- elem :: (Foldable t, Eq a) => a -> t a -> Bool
-- As with Functor, we can't map over the Left data constructor in Either since the left type argument (a) is part of the structure
-- Likewise, elem can't see inside the Left constructor, so the result will be False even if the value matches.

-- λ> elem 2 (Just 3)
-- False
-- λ> elem True (Left False)
-- False
-- λ> elem True (Left True)
-- False
-- λ> elem True (Right False)
-- False
-- λ> elem True (Right True)
-- True
-- λ> fmap (elem 3) [Right 1, Right 2, Right 3]
-- [False,False,True]


-- Largest element of non-empty structure
-- maximum :: (Foldable t, Ord a) => t a -> a

-- λ> maximum [10, 12, 33, 5]
-- 33
-- fmap :: (Maybe a -> a) -> [Maybe a] -> [a]
-- λ> fmap maximum [Just 2, Just 10, Just 4]
-- [2,10,4]
-- λ> fmap maximum (Just [3, 7, 10, 2])
-- Just 10
-- λ> fmap minimum [Just 4, Just 3, Nothing]
-- [4,3,*** Exception: minimum: empty structure
-- λ> minimum Nothing
-- *** Exception: minimum: empty structure
-- λ> minimum (Left 3)
-- *** Exception: minimum: empty structure


-- Least element of non-empty structure
-- minimum :: (Foldable t, Ord a) => t a -> a

-- λ> minimum "julie"
-- 'e'
-- λ> fmap minimum (Just "julie")
-- Just 'e'
-- λ> fmap minimum $ map Just "jul"
-- "jul"


-- sum :: (Foldable t, Num a) => t a -> a
-- sum = getSum #. foldMap Sum (from source)

-- λ> sum (7, 5)
-- 5
-- λ> fmap sum [(7, 5), (3, 4)]
-- [5,4]
-- λ> fmap sum (Just [1, 2, 3, 4, 5])
-- Just 15
-- λ> sum Nothing
-- 0


-- product :: (Foldable t, Num a) => t a -> a
-- product = getProduct #. foldMap Product (from source)

-- λ> product Nothing
-- 1
-- λ> fmap product (Just [])
-- Just 1
-- λ> fmap product (Right [1, 2, 3])
-- Right 6
