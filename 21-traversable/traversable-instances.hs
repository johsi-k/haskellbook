-- Either

-- data Either a b =
--     Left a
--   | Right b
--   deriving (Eq, Ord, Show)

-- instance Functor (Either a) where
  -- fmap :: (x -> y) -> Either a x -> Either a y
  -- fmap _ (Left x) = Left x
  -- fmap f (Right y) = Right (f y)

-- instance Applicative (Either e) where
  -- pure          = Right

  -- (<*>) :: Either a (x -> y) -> Either a x -> Either a y
  -- Left e <*> _  = Left e
  -- Right f <*> r = fmap f r

-- instance Foldable (Either a) where
  -- foldMap :: Monoid m => (x -> m) -> Either a x -> m
  -- foldMap _ (Left _) = mempty
  -- foldMap f (Right y) = f y

  -- foldr :: (x -> b -> b) -> b -> Either a x -> b
  -- foldr _ z (Left _) = z
  -- foldr f z (Right y) = f y z

-- instance Traversable (Either a) where
  -- traverse :: Applicative f => (x -> f y) -> Either a x -> f (Either a y)
  -- traverse _ (Left x) = pure (Left x)
  -- traverse f (Right y) = fmap Right (f y)


-- Tuple

-- instance Functor ((,) a) where
  -- fmap :: (x -> y) -> (,) a x -> (,) a y
  -- fmap f (x, y) = (x, f y)

-- instance Monoid a => Applicative ((,) a) where
  -- pure :: x -> (,) a x
  -- pure x = (mempty, x)

  -- (<*>) :: (,) a (x -> y) -> (,) a x -> (,) a y
  -- (u, f) <*> (v, x) = (u <> v, f x)

-- instance Foldable ((,) a) where
  -- foldMap :: Monoid m => (x -> m) -> (,) a x -> m
  -- foldMap f (_, y) = f y

  -- foldr :: (x -> y -> y) -> y -> (,) a x -> y
  -- foldr f z (_, y) = f y z

-- instance Traversable ((,) a) where
  -- traverse :: Applicative f => (x -> f y) -> (,) a x -> f ((,) a y)
  -- traverse f (x, y) = (,) x <$> f y
