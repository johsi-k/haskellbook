import System.Random
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State

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

-- randomR :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)


rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _)  = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)


rollDieBind :: State StdGen Die
rollDieBind = state $
  randomR (1, 6) >>= (\(n, s) -> return (intToDie n, s))

-- randomR :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)
-- randomR :: (Int, Int) -> StdGen -> (Int, StdGen)

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- a ~ (Int, StdGen)
-- m ~ ((->) StdGen)
-- (>>=) :: (StdGen -> (Int, StdGen))
--       -> ((Int, StdGen) -> (StdGen -> (Die, StdGen)))
--       -> (StdGen -> (Die, StdGen))

-- state :: Monad m => (s -> (a, s)) -> StateT s m a
-- state :: (StdGen -> (Die, StdGen)) -> StateT StdGen m Die
-- m ~ Identity
-- state :: (StdGen -> (Die, StdGen)) -> StateT StdGen Identity Die
-- state :: (StdGen -> (Die, StdGen)) -> State StdGen Die

-- type State s          = StateT s      Identity
-- type State s      a   = StateT s      Identity a
-- type State StdGen Die = StateT StdGen Identity Die


-- We can lift intToDie over the State
rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

-- randomR :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)
-- randomR :: (Int, Int) -> StdGen -> (Int, StdGen)
-- randomR (1, 6) :: StdGen -> (Int, StdGen)

-- state :: Monad m => (s -> (a, s)) -> StateT s m a
-- state :: (StdGen -> (Int, StdGen)) -> StateT StdGen m Int
-- m ~ Identity
-- state :: (StdGen -> (Int, StdGen)) -> State StdGen Int

-- state f = StateT (return . f)
-- (.) :: (y -> z) -> (x -> y) -> x -> z
-- return :: Monad m => b -> m b
-- f :: StdGen -> (Int, StdGen)
-- y ~ b, z ~ m b
-- (.) :: (b -> m b) -> (x -> b) -> x -> m b
-- return (.) :: (x -> b) -> x -> m b
-- x -> StdGen b ~ (Int, StdGen)
-- return (.) :: (StdGen -> (Int, StdGen)) -> StdGen -> m (Int, StdGen)
-- (return . f) :: Monad m => StdGen -> m (Int, StdGen)

-- StateT :: (s ->      m (a,   s))     -> StateT s       m a
-- StateT :: (StdGen -> m (Int, StdGen)) -> StateT StdGen m Int

-- (<$>) :: Applicative f => (a -> b) -> f a -> f b
-- f ~ State StdGen, a ~ Int, b ~ Die
-- (<$>) :: (Int -> Die)
--       -> State StdGen Int
--       -> State StdGen Die

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

-- liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-- (,,) :: x -> y -> z -> (x, y, z)
-- rollDie :: State StdGen Die
-- x = y = z ~ Die
-- f ~ State StdGen
-- liftA3 :: (Die -> Die -> Die -> (Die, Die, Die))
--        -> State StdGen Die
--        -> State StdGen Die
--        -> State StdGen Die
--        -> State StdGen (Die, Die, Die)

-- evalState :: State s a -> s -> a
-- evalState :: State StdGen (Die, Die, Die) -> StdGen -> (Die, Die, Die)

-- λ> evalState rollDieThreeTimes' (mkStdGen 0)
-- (DieSix,DieSix,DieFour)
-- λ> evalState rollDieThreeTimes' (mkStdGen 1)
-- (DieSix,DieFive,DieTwo)

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

-- repeat :: a -> [a]
-- fmap :: (a -> b) -> f a -> f b
-- f ~ State StdGen
-- fmap :: (Die -> [Die]) -> State StdGen Die -> State StdGen [Die]

-- λ> take 6 $ evalState infiniteDie gen
-- [DieSix,DieSix,DieSix,DieSix,DieSix,DieSix]

-- But this only repeats a single die value. We want to repeat the state action that produces a die.

-- replicateM has what we need
nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

-- replicateM :: Applicative m => Int -> m a -> m [a]
-- m ~ State StdGen
-- replicateM :: Int -> State StdGen Die -> State StdGen [Die]

-- 'replicateM 5' can be understood as:
-- do a1 <- as
--    a2 <- as
--    a3 <- as
--    a4 <- as
--    a5 <- as
--    pure [a1,a2,a3,a4,a5]


-- Rolling a die until we reach or exceed a sum of 20
rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum' count gen -- initial sum = 0, count = 0
      | sum' >= 20 = count -- return count if sum >=20
      | otherwise =
        -- (die, nextGen) :: (Int, StdGen)
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum' + die) (count + 1) nextGen

-- randomR :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)
-- randomR :: (Int, Int) -> StdGen -> (Int, StdGen)


-- Exercises: Roll your own
-- exercises.hs

-- 1. Refactor rollsToGetTwenty so that the limit is an argument to the function
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
