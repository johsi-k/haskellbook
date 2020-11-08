import Data.Functor.Identity
import Data.Functor.Constant

-- Typeclass definition
-- class (Functor t, Foldable t) => Traversable where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
--   traverse f = sequenceA . fmap f

-- traverse maps each element of a structure to an action, evaluates the actions from left to right and collects the results.
-- If we wind up with a type like [IO a], it's likely we made a mistake by using fmap where we should have used traverse:

-- myData :: [String]
-- myFunc :: String -> IO Record

-- wrong :: [IO Record]
-- wrong = fmap myFunc myData
-- fmap :: Functor f :: (a -> b) -> f a -> f b
-- fmap :: (String -> IO Record) -> [String] -> [IO Record]

-- right :: IO [Record]
-- right = traverse myFunc myData
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- traverse :: (String -> IO Record) -> [String] -> IO [Record]


-- sequenceA is the counterpart to traverse
--   sequenceA :: Applicative f => t (f a) -> f (t a)
--   sequenceA = traverse id


-- extra stuff
-- (fmap . fmap)
--   :: (Functor f1, Functor f2) => (a -> Char) -> f1 (f2 a) -> f1 (f2 Char)

-- fmap :: (String -> Char) -> Maybe String -> Maybe Char
-- fmap replaceWithP :: Maybe String -> Maybe Char

-- fmap (fmap replaceWithP) :: (Maybe String -> Maybe Char) -> f Maybe String -> f Maybe Char
-- fmap (Maybe String -> Maybe Char) -> [Maybe String] -> [Maybe Char]


-- sequenceA is used to flip two contexts or structures

-- compare:
-- λ> sum [1, 2, 3]
-- 6
-- λ> fmap sum [Just 1, Just 2, Just 3]
-- [1, 2, 3]
-- λ> fmap product [Just 1, Just 2, Nothing]
-- [1, 2, 1]

-- λ> (fmap . fmap) sum Just [1, 2, 3] -- note the lack of $ before Just
-- Just 6
-- (fmap . fmap) :: (Functor f1, Functor f2, Foldable t, Num a)
--               => (t a -> a)
--               -> (t a -> Maybe (t a))
--               -> (t a -> Maybe a)

-- with:
-- λ> sequenceA [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- λ> sequenceA [Just 1, Just 2, Nothing]
-- Nothing
-- λ> fmap sum $ Just [1, 2, 3]
-- Just 6
-- λ> fmap product Nothing
-- Nothing


-- Data.Maybe has a function catMaybes that allows us to process a list of Maybe values even if there is potentially a Nothing lurking within.

-- λ> catMaybes [Just 1, Just 2, Just 3]
-- [1,2,3]
-- λ> catMaybes [Just 1, Just 2, Nothing]
-- [1,2]
-- λ> sum $ catMaybes $ [Just 1, Just 2, Just 3] ++ [Nothing]
-- 6
-- λ> fmap sum $ sequenceA $ [Just 1, Just 2, Just 3] ++ [Nothing]
-- Nothing


-- traverse
-- traverse :: (Applicative f, Traversable t)
--          => (a -> f b)
--          -> t a
--          -> f (t b)

-- fmap     :: (a -> b)   -> f a -> f b
-- (=<<)    :: (a -> m b) -> m a -> m b
-- traverse :: (a -> f b) -> t a -> f (t b)

-- like fmap, traverse maps a function over some embedded value
-- but like flip bind, the function generates more structure
-- unlike flip bind, the structure can be of a different type than the structure lifted over to apply the function
-- in the end the structures are flipped around using sequenceA

-- traverse f = sequenceA . fmap f

-- λ> fmap Just [1, 2, 3]
-- [Just 1, Just 2, Just 3]

-- λ> sequenceA $ fmap Just [1, 2, 3]
-- Just [1, 2, 3]

-- λ> sequenceA . fmap Just $ [1, 2, 3]
-- Just [1, 2, 3]

-- sequenceA :: t (f a) -> f (t a)
-- fmap :: (a -> b) -> f a -> f b
-- fmap :: (a -> Maybe a) -> f a -> f (Maybe a)
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (.) :: ([Maybe a] -> Maybe [a]) -> ([a] -> [Maybe a]) -> [a] -> Maybe [a]

-- λ> traverse Just [1, 2, 3]
-- Just [1, 2, 3]


-- mapM is traverse
-- mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

-- likewise,
-- sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
-- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)


-- What Traversable is for
-- f = undefined :: a -> Maybe b
-- xs = undefined :: [a]
-- map f xs :: [Maybe b]

-- If we wanted a value of type Maybe [b] instead,
-- sequenceA $ map f xs :: Maybe [a]

-- It's usually better to use traverse when we have a sequence/sequenceA combined with map/fmap
-- traverse f xs :: Maybe [b]


-- Morse code revisited
-- see morse.hs


-- Axing tedious code
data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

-- decoder function that makes some object from a string
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

-- query that runs against a database and returns an array of strings
fetchFn :: Query -> IO [String]
fetchFn = undefined

-- an additional "context initializer" that performs IO
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

-- putting them together
pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query -- a :: [String]
  case sequence (map decodeFn a) of
    -- map decodeFn a :: [Either Err SomeObj]
    -- sequence $ map decodeFn a :: Either Err [SomeObj]
    (Left err) -> return $ Left err
    (Right res) -> do -- res :: [SomeObj]
      a <- makeIoOnlyObj res -- a :: [(SomeObj, IoOnlyObj)]
      return $ Right a

-- Could be improved:
-- use of sequence and map
-- manually casing on the result of sequence and map
-- binding monadically over Either only to perform another monadic IO action inside it

-- Paring it down:
pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
-- pipelineFn' query = do
--   a <- fetchFn query
--   traverse makeIoOnlyObj (mapM decodeFn a)

-- point-free
pipelineFn' query =
  -- fetchFn query >>= (\a -> traverse makeIoOnlyObj (mapM decodeFn a))
  -- fetchFn query >>= ((traverse makeIoOnlyObj) . (mapM decodeFn))
  -- ((traverse makeIoOnlyObj) . (mapM decodeFn)) =<< fetchFn query
  -- ((traverse makeIoOnlyObj) . (mapM decodeFn)) =<< fetchFn query
   -- (((traverse makeIoOnlyObj) . (mapM decodeFn)) =<<) (fetchFn query)
   ((((traverse makeIoOnlyObj) . (mapM decodeFn)) =<<) . fetchFn) query


-- Strength for understanding
-- We can recover the Functor and Foldable instance for a type from the Traversable

-- Here we use the Identity type to get something that is essentially fmap:
-- λ> traverse (Identity . (+1)) [1, 2]
-- Identity [1,2]
-- λ> runIdentity $ traverse (Identity . (+1)) [1, 2]
-- [2,3]

edgeMap :: Traversable t => (a -> b) -> t a -> t b
edgeMap f t = runIdentity $ traverse (Identity . f) t

-- λ> edgeMap (+1) [1..5]
-- [2,3,4,5,6]


-- Likewise we can use Const/Constant to recover a foldMap-like Foldable
-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m

foldMap' :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f t = getConstant $ traverse (Constant . f) t
