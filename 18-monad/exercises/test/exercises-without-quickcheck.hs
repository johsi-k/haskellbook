import Control.Monad

-- The answer is the exercise
-- Write bind in terms of fmap and join

-- fmap :: Applicative f => (a -> b) -> f a -> f b
-- join :: Monad m => m (m a) -> m a
-- fmap :: Monad m => (a -> m b) -> m a -> m (m b)
-- join :: Monad m => m (m b) -> m b

-- this is (>>=) flipped, i.e. (=<<)
bind :: Monad m => (a -> m b) -> m a -> m b
-- bind f ma = join $ fmap f ma
bind = (join .) . fmap


-- Short Exercise: Either Monad
-- Implement the Either Monad

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a -- a-type is part of functorial structure
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  -- pure :: Applicative f => a -> f a
  -- f ~ Sum a; Second :: b -> Sum a b
  pure = Second

  -- <*> :: Applicative f => f (a -> b) -> f a -> f b
  -- <*> :: Applicative f => Sum a (x -> y) -> Sum a x -> Sum a y
  -- First :: a -> Sum a b, Second :: b -> Sum a b
  (<*>) (First x) _ = First x
  (<*>) (Second f) (Second x) = Second (f x)
  (<*>) (Second f) x = fmap f x

instance Monad (Sum a) where
  return = pure

  -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
  -- (>>=) :: Sum a x -> (x -> Sum a y) -> Sum a y
  (>>=) (First x) _ = First x
  (>>=) (Second x) f = f x


-- Chapter Exercises

-- Write the following functions using the methods provided by Monad and Functor. Using stuff like identity and composition is fine, but it has to typecheck with types provided.

-- 1.
j :: Monad m => m (m a) -> m a
j = join

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap

-- 5. You'll need recursion for this one.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = (:) <$> f x <*> meh xs f

-- xs :: [a], x :: a
-- f x :: m b
-- meh takes inputs :: [a] and (a -> m b)

-- (:) :: a -> [a] -> [a]
-- fmap :: (x -> y) -> m x -> m y
-- fmap :: (b -> ([b] -> [b])) -> m b -> m ([b] -> [b])
-- ap/<*> :: m ([b] -> [b]) -> m [b] -> m [b]

-- alternatively,
-- meh' :: Monad m => [a] -> (a -> m b) -> m [b]
-- meh' [] _ = return []
-- meh' (a:as) f = do
--   b <- f a
--   bs <- meh as f
--   return (b:bs)
-- meh' (a:as) f = f a >>= (\b -> meh as f >>= (\bs -> return (b:bs)))


-- 6. Hint: reuse "meh"
flipType :: (Monad m) => [m a] -> m [a]
flipType mas = meh mas id

-- flipType :: (Monad m) => [m a] -> m [a]
-- meh :: Monad m => [c] -> (c -> m a) -> m [a]
-- c ~ m a
-- meh :: Monad m => [m a] -> (m a -> m a) -> m [a]
