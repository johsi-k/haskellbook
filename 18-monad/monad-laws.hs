import Control.Monad

-- Identity laws

-- right identity
-- m >>= return = m

-- return :: a -> m a
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
--                      m  >>=  return   =   m
-- (>>=) :: Monad m => m a -> (a -> m a) -> m a

-- left identity
-- return x >>= f = f x

-- (>>=) :: Monad m => m a      ->  (a -> m b) -> m b
--                     return x >>=    f       =  f x

-- Both laws assert that return should be neutral and not perform any computation


-- Associativity
-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)


-- Application and Composition

-- Composition under functors and applicatives just worked, but the situation is less neat with monads

-- Defining composition for monadic functions:
mcomp :: Monad m
      => (b -> m c) -- f
      -> (a -> m b) -- g
      -> a
      -> m c
mcomp f g a = g a >>= f


-- Kleisli composition

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- flipping the order of args to match >>=,
-- (>=>) :: (a -> m b) -> (b -> m c) -> a -> m c

-- looks similar to
-- flip (.) :: (a -> b) -> (b -> c) -> a -> c

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

-- (>>) :: IO () -> IO String -> IO String
-- sayHi greeting = putStrLn greeting >> getLine

readM :: Read a => String -> IO a
readM = return . read

-- return :: Monad m => a -> m a
-- read :: Read a => String -> a
-- (.) ::           (y -> z)    -> (x      -> y) -> x      -> z
-- (.) :: Read a => (a -> IO a) -> (String -> a) -> String -> IO a

getAge :: String -> IO Int
getAge = sayHi >=> readM

-- (>=>) :: Monad m => (a      -> m  b)      -> (b      -> m  c)   -> a      -> m  c
-- (>=>) ::            (String -> IO String) -> (String -> IO Int) -> String -> IO Int

askForAge :: IO Int
askForAge = getAge "Hello! How old are you?"
