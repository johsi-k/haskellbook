import Data.Monoid

-- Exercises: Library Functions
-- Implement the functions in terms of foldMap or foldr from Foldable, then try them out with multiple types that have Foldable instances.

-- 1. This and the next one are nicer with foldMap, but foldr is fine too.
sum' :: (Foldable t, Num a) => t a -> a
-- sum' = getSum <$> foldMap Sum
sum' = getSum . foldMap Sum

-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- Sum :: a -> Sum a, m ~ Sum a
-- foldMap :: Foldable t => (a -> Sum a) -> t a -> Sum a

-- fmap :: (a -> b) -> f a -> f b
-- f ~ ((->) t a)
-- fmap :: (Sum a -> a) -> (t a -> Sum a) -> (t a -> a)


-- 2.
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- 3.
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a = foldr (\x acc -> x == a || acc) False


-- 4.
-- minimum' :: Ord a => [a] -> Maybe a
-- minimum' [] = Nothing
-- minimum' (x:xs) = Just $ foldr rf x xs
--   where rf a acc
--           | a < acc = a
--           | otherwise = acc

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr rf Nothing
  where
    rf x Nothing = Just x -- accumulator is Nothing
    rf x (Just y) -- accumulator is a Just
      | x < y = Just x
      | otherwise = Just y


-- 5.
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr rf Nothing
  where
    rf x Nothing = Just x
    rf x (Just y) | x > y = Just x
                  | otherwise = Just y


-- 6.
null' :: (Foldable t) => t x -> Bool
-- null' = foldr (\_ _ -> False) True
null' = foldr ((const . const) False) True


-- 7.
length' :: (Foldable t) => t a -> Int
-- length' = foldr (\_ acc -> acc + 1) 0
length' = foldr (const (1+)) 0


-- 8.
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []


-- 9. Hint: use foldMap.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id


-- 10. Define foldMap in terms of foldr
-- foldr :: (a -> (b -> b)) -> b -> t a -> b
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x acc -> f x <> acc) mempty


-- Chapter Exercises
-- Write Foldable instances for the following datatypes.

-- 1.
data Constant a b = Constant b deriving (Eq, Show)

instance Foldable (Constant a) where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  -- foldr :: (b -> c -> c) -> c -> Constant a b -> c
  foldr f z (Constant b) = f b z
  foldl f z (Constant b) = f z b
  foldMap f (Constant b) = f b


-- 2.
data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  -- foldr :: (b -> c -> c) -> c -> Two a b -> c
  foldr f z (Two _ b) = f b z
  foldl f z (Two _ b) = f z b
  foldMap f (Two _ b) = f b


-- 3.
data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  -- foldr :: (c -> d -> d) -> d -> Three a b c -> d
  foldr f z (Three _ _ c) = f c z
  foldl f z (Three _ _ c) = f z c
  foldMap f (Three _ _ c) = f c


-- 4.
data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  -- foldr :: (b -> c -> c) -> c -> Three' a b -> c

  -- foldr f z (Three' _ b1 b2) = f b1 (f b2 z)
  foldr f z (Three' _ b1 b2) = (f b1 . f b2) z

  -- foldl f z (Three' _ b1 b2) = f (f z b1) b2
  foldl f z (Three' _ b1 b2) =
    (flipf b1 . flipf b2) z where flipf = flip f

  -- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
  -- foldMap :: (b -> m) -> Three' a b -> m
  foldMap f (Three' _ b1 b2) = f b1 <> f b2

-- foldr has a Monoid in Endo - see that f is (a -> (b -> b))
-- Endo is an instance of Monoid with composition:
-- instance Monoid (Endo a) where
--   mempty = Endo id
--   Endo f `mappend` Endo g = Endo (f . g)
-- https://stackoverflow.com/questions/43645939/how-can-i-understand-the-laws-for-foldable-instances


-- 5.
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  -- foldr :: (b -> c -> c) -> c -> Four' a b -> c

  foldr f z (Four' _ b1 b2 b3) = (f b1 . f b2 . f b3) z

  foldl f z (Four' _ b1 b2 b3) =
    (flipf b1 . flipf b2 . flipf b3) z where flipf = flip f

  foldMap f (Four' _ b1 b2 b3) = f b1 <> f b2 <> f b3


-- Thinking cap time. Write a filter function for Foldable types using foldMap.
filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF cond = foldMap mf
  where mf a | cond a = pure a
             | otherwise = mempty
