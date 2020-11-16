-- import Control.Monad
import Control.Monad.Trans.State
import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
  execState (mapM_ addResult list) []

-- mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
-- a ~ Integer, t ~ [], m b ~ State [String] (), m ~ State [String]
-- mapM_ :: (Integer -> State [String] ())
--       -> [Integer]
--       -> State [String] ()

-- mapM_ addResult list = foldr c (return ()) list
--   where c x k = addResult x >> k

-- foldr f z []     = z
-- foldr f z (x:xs) = f x (foldr f z xs)

-- mapM_ addResult [1, 2, 3]
-- = foldr c (return ()) [1, 2, 3]
-- = c 1 (c 2 (c 3 (foldr c (return ()) [])))
-- = c 1 (c 2 (c 3 (return ())))
-- = addResult 1 >> (addResult 2 >> (addResult 3 >> return ()))

-- p >>= k = state $ \ s0 ->
--   let (x, s1) = runState p s0
--   in runState (k x) s1

--   runState (k x) s1
-- = runState ((\_ -> return ()) x) s1
-- = runState (return ()) s1
-- = ((), s1)

-- addResult 3 >> return () =
-- addResult 3 >>= \_ -> return () =
--   state $ \s0 -> ((), s1)
--     where (_, s1) = runState (addResult 3) s0

-- addResult 2 >> (...) =
-- addResult 2 >>= (\_ -> ...) = state $ \s ->
--   runState (k x) s'
--     where
--       k = (\_ -> state $ \s0 -> ((), s1) where (_, s1) = runState (addResult 3) s0)
--       (x, s') = runState (addResult 2) s

-- addResult 2 >>= (\_ -> ...) = state $ \s ->
--   runState k s'
--     where
--       k = state $ \s0 -> ((), s1)
--         where (_, s1) = runState (addResult 3) s0
--       (x, s') = runState (addResult 2) s

-- addResult 2 >>= (\_ -> ...) = state $ \s ->
--   runState k s'
--     where
--       k = state $ \s0 -> ((), s1)
--         where (_, s1) = runState (addResult 3) s0
--       (_, s') = runState (addResult 2) s

-- addResult 2 >>= (\_ -> ...) = state $ \s ->
--   stateFn s'
--     where
--       stateFn = \s0 -> ((), s1)
--         where (_, s1) = runState (addResult 3) s0
--       (_, s') = runState (addResult 2) s

-- applying stateFn to s':
-- addResult 2 >>= (\_ -> ...) = state $ \s ->
--   ((), s1)
--     where
--       (_, s1) = runState (addResult 3) s'
--       (_, s') = runState (addResult 2) s

-- renaming s and s':
-- addResult 2 >>= (\_ -> ...) = state $ \s0 ->
--   ((), s2)
--     where
--       (_, s2) = runState (addResult 3) s1
--       (_, s1) = runState (addResult 2) s0

-- applying the same pattern:
-- addResult 1 >> (...) =
-- addResult 1 >> (\_ ...) = state $ \s0 ->
--   ((), s3)
--     where
--       (_, s3) = runState (addResult 3) s2
--       (_, s2) = runState (addResult 2) s1
--       (_, s1) = runState (addResult 1) s0

-- execState (addResult 1 >> (addResult 2 >> (addResult 3 >> return ()))) []
-- ["Fizz", "2", "1"]

-- return a = StateT $ \ s -> return (a, s)
-- return () = StateT $ \s -> return ((), s) :: Monad m => StateT b m ()

-- execState :: State s a -> s -> s
-- execState :: State [String] ()
--           -> [String]
--           -> [String]
-- execState m s = snd (runState m s)

-- runState :: State s a -> s -> (a, s)
-- runState :: State [String] () -> [String] -> ((), [String])
-- runState m = runIdentity . runStateT m

-- runStateT :: StateT s m a -> s -> m (a, s)
-- runStateT m :: s -> m (a, s)
-- runIdentity :: Identity a -> a
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- a ~ s, b ~ Identity (a, s), c ~ (a, s)
-- (.) :: (Identity (a, s) -> (a, s))
--     -> (s -> Identity (a, s))
--     -> s -> (a, s)


addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)
-- addResult n = get >>= (\xs -> put (fizzBuzz n : xs))

-- get fetches the current value of the state by setting the result value to the state
-- get :: Monad m => StateT s m s
-- get = state $ \s -> (s, s)  -- a is set to s
-- m ~ Identity, s ~ [String]
-- get :: StateT [String] Identity [String]
-- get :: State [String] [String]

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- a ~ [String], m ~ State [String], b ~ ()
-- (>>=) :: State [String] [String]
--       -> [String] -> State [String] ()
--       -> State [String] ()

-- put sets the state within the monad to s (arg) and the result value to ()
-- put :: Monad m => s -> StateT s m ()
-- put s = state $ \_ -> ((), s)
-- put :: [String] -> StateT [String] m ()
-- m ~ Identity
-- put :: [String] -> StateT [String] Identity ()
-- put :: [String] -> State [String] ()

-- type State s a = StateT s Identity a


-- with DList
fizzbuzzList' :: [Integer] -> [String]
fizzbuzzList' list =
  let dlist = execState (mapM_ addResult' list) DL.empty
  in DL.apply dlist []

addResult' :: Integer -> State (DL.DList String) ()
addResult' n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)


-- Exercise: FizzBuzz differently
-- see exercises.hs


main :: IO ()
main = do
  mapM_ putStrLn $
    reverse $ fizzbuzzList [1..100]

  mapM_ putStrLn $ fizzbuzzList' [1..100]
