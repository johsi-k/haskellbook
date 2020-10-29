import Control.Monad (join)

-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a

-- Monad is stronger than Applicative, and Applicative is stronger than Functor
-- Just as we can derive Functor in terms of Applicative, we can derive Applicative and Functor in terms of Monad
-- This means we can write fmap using monadic operations
-- e.g. fmap f xs = xs >>= return . f


-- The novel part of Monad
-- Not return, which is another name for pure from Applicative
-- Not >>, which also has a counterpart from Applicative
-- Not >>= either, whose type is visibly similar to fmap and <*>.

--The idea of mapping a function over a value while bypassing structure is not unique to Monad.

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- if b == f b
-- fmap :: Functor f => (a -> f b) -> f a -> f (f b)

andOne :: Integer -> [Integer]
andOne x = [x, 1]

-- λ> andOne 10
-- [10,1]

-- λ> fmap andOne [4, 5, 6]
-- [[4,1],[5,1],[6,1]]

-- Now we have a result of nested lists. After mapping a function that generates additional monadic structure in the return type, we want to discard one layer of that structure.
-- We can do that with concat
-- concat :: Foldable t => t [a] -> [a]

-- Monad is a generalization of concat! The unique bit about Monad is its join function
-- By putting the join function together with the mapping function we get bind (>>=)


-- The answer is the exercise
-- see exercises-without-quickcheck.hs


-- Do syntax and monads

-- (*>) :: Applicative f => f a -> f b -> f b
-- (>>) :: Monad m => m a -> m b -> m b

-- λ> putStrLn "Hello, " >> putStrLn "World!"
-- Hello,
-- World!

-- λ> putStrLn "Hello, " *> putStrLn "World!"
-- Hello,
-- World!

-- We can see desugared do looks like by manually transforming it ourselves

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
  putStrLn "blah" >>
  putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' =
  putStrLn "blah" *>
  putStrLn "another thing"

-- We can do the same with the variable binding that do syntax includes

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' =
  getLine >>= putStrLn

-- Instead of naming the variable and passing that as an argument to the next function, we use >>= which passes it directly.


-- When fmap alone isn't enough

-- λ> putStrLn <$> getLine
-- This will not print whatever input it is given

-- getLine :: IO String
-- putStrLn :: String -> IO ()
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- fmap :: (String -> IO ()) -> IO String -> IO (IO ())

-- The outermost IO structure represents the effects getLine must perform to get a String that the user typed in
-- The inner IO structure represents the effects that would be performed if putStrLn were evaluated
-- The unit () is the unit which putStrLn returns

-- For a simpler example of how we can wait to evaluate IO actions/any other computation:
printOne :: IO ()
printOne = putStrLn "1"

printTwo :: IO ()
printTwo = putStrLn "2"

twoActions :: (IO (), IO ())
twoActions = (printOne, printTwo)

-- IO actions can be evaluated multiple times - this will be significant later.

-- To join those two IO layers together, we need the monadic join

-- join $ putStrLn <$> getLine :: IO ()
-- λ> join $ putStrLn <$> getLine
-- blah
-- blah


bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >>
  getLine >>=
  \name ->
    putStrLn ("y helo thar: " ++ name)

-- putStrLn "name pls:" >> (getLine ..)
-- (>>) :: m a -> m b -> m b
-- (>>) :: IO () -> IO () -> IO ()

-- getLine >>= (\name -> ..)
-- (>>=) :: m a -> (a -> m b) -> m b
-- (>>=) :: IO String -> (String -> IO ()) -> IO ()


twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn ("y helo thar: "
            ++ name ++ " who is: "
            ++ age ++ " years old.")

-- twoBinds' :: IO ()
-- twoBinds' =
--   (putStrLn "name pls:") >>
--   (getLine >>=
--   \name ->
--     ((putStrLn "age pls:") >>
--     (getLine >>=
--     \age ->
--       putStrLn ("y helo thar: "
--                 ++ name ++ " who is: "
--                 ++ age ++ " years old."))))

twoBinds' :: IO ()
twoBinds' =
  requestName >>
  (getLine >>= \name -> (requestAge >>
                         (getLine >>= \age -> output name age)))
  where requestName = putStrLn "name pls:"
        requestAge = putStrLn "age pls:"
        output n a = putStrLn ("y helo thar: "
                                ++ n ++ " who is :"
                                ++ a ++ " years old.")

-- requestName >> (getLine ..)
-- (>>) :: m a -> m b -> m b
-- (>>) :: IO () -> IO b -> IO b
-- (>>) :: IO () -> IO () -> IO ()

-- getLine >>= (\name ..)
-- (>>=) :: m c -> (c -> m d) -> m d
-- (>>=) :: IO String -> (String -> IO d) -> IO d
-- (>>=) :: IO String -> (String -> IO ()) -> IO ()

-- requestAge >> (getLine ..)
-- (>>) :: m e -> m f -> m f
-- (>>) :: IO () -> IO f -> IO f
-- (>>) :: IO () -> IO () -> IO ()

-- getLine >>= (\age -> ..)
-- (>>=) :: m g -> (g -> m h) -> m h
-- (>>=) :: IO String -> (String -> IO h) -> IO h
-- (>>=) :: IO String -> (String -> IO ()) -> IO ()

-- twoBinds' :: IO (), so b = ()
-- d = b, so d = ()
-- (\name ..) :: String -> IO (), so f = ()
-- (getLine >>= \age ..) :: IO (), so h = ()
