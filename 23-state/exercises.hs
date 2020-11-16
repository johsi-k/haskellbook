{-# LANGUAGE InstanceSigs #-}
import System.Random (StdGen, randomR)
import qualified Control.Monad.Trans.State as S (State, get, put, execState)

-- Exercises: Roll your own

-- 1. Refactor rollsToGetTwenty so that the limit is an argument to the function

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix

   -- do not use error outside experiments like this
   -- or where the branch you're ignoring is provably impossible
    x -> error $
      "intToDie got non 1-6 integer: "
      ++ show x

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN limit = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum' count gen
      | sum' >= limit = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum' + die) (count + 1) nextGen

-- λ> (rollsToGetN 10) . mkStdGen <$> randomIO
-- 3


-- 2. Change rollsToGetN to record the series of dice that are rolled, in addition to the count of the total number of rolls:

rollsCountLogged :: Int
                 -> StdGen
                 -> (Int, [Die])
rollsCountLogged limit = go 0 []
  where
    go :: Int -> [Die] -> StdGen -> (Int, [Die])
    go sum' dice gen
      | sum' >= limit = (length dice, dice)
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go  (sum' + die) (intToDie die : dice) nextGen

-- λ> (rollsCountLogged 10) . mkStdGen <$> randomIO
-- (2,[DieFive,DieFive])


-- Write State for yourself
newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

-- State Functor
instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  -- g :: (s -> (a, s))
  fmap f (Moi g) = Moi $ \s ->
    let (a, ns) = g s
    in (f a, ns)

-- State Applicative
instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  -- f :: (s -> ((a -> b), s))
  -- g :: (s -> (a, s))
  (Moi f) <*> (Moi g) = Moi $ \s ->
    let (fab, s')    = f s
        (a,   s'')   = g s'
    in (fab a, s'')

-- State Monad
instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s ->
    let (a, ns) = f s
    in runMoi (g a) ns


-- FizzBuzz differently
-- Continue to use consing in the construction of the result list, but have it come out in the right order to begin with by enumerating the sequence backward.

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

addResult :: Integer -> S.State [String] ()
addResult n = do
  xs <- S.get
  let result = fizzBuzz n
  S.put (result : xs)

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = S.execState (mapM_ addResult [to, (pred to)..from]) []

main :: IO ()
main = mapM_ putStrLn $ fizzbuzzFromTo 1 100


-- Chapter exercises
-- Write the following functions. You'll want to use your own State type for which you've defined Functor, Applicative and Monad instances.

-- 1. Construct a State where the state is also the value you return
get :: Moi s s
get = Moi $ \s -> (s, s)


-- 2. Construct a State where the resulting state is the argument provided, and the value defaults to unit
put :: s -> Moi s ()
put s = Moi $ const ((), s)


-- 3. Run the State with s and get the state that results
exec :: Moi s a -> s -> s
exec (Moi sa) = snd . sa


-- 4. Run the State with s and get the value that results
eval :: Moi s a -> s -> a
eval (Moi sa) = fst . sa


-- 5. Write a function that applies a function to create a new State
modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)
