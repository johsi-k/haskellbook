import System.Random

-- Random numbers

-- System.Random is designed to generate pseudorandom values

-- class RandomGen g where
--   next :: g -> (Int, g)
--   genRange :: g -> (Int, Int)
--   split :: g -> (g, g)
--   {-# MINIMAL next, split #-}

-- newtype StdGen = StdGen { unStdGen :: SM.SMGen }
-- StdGen is a datatype that is a product of two Int32 values. These are seed values used to generate the next random number. StdGen is the standard RandomGen instance provided by the library.

-- | Constructs a 'StdGen' deterministically.
-- mkStdGen :: Int -> StdGen
-- mkStdGen = StdGen . SM.mkSMGen . fromIntegral
-- mkStdGen takes an Int arg and maps it onto a generator to return a value of type StdGen - a pair of Int32 values

-- [deprecated]
-- next :: g -> (Int, g)
-- Where g is type var that specialises to StdGen. The first member of the tuple - of type Int - is the pseudorandom number generated from the StdGen value. The second member is a new StdGen value.

-- random :: (RandomGen g, Random a) => g -> (a, g)
-- Similar to next but allows us to generate random values that aren't numbers. The range generated will be determined by the type.


-- A little demonstration

sg :: StdGen
sg = mkStdGen 0

-- λ> sg
-- 1 1

-- λ> :t next sg
-- next sg :: (Int, StdGen)

-- λ> next sg
-- (2147482884,40014 40692)
-- λ> next sg
-- (2147482884,40014 40692)


newSg :: StdGen
newSg = snd (next sg)

-- λ> newSg
-- 40014 40692

-- λ> next newSg
-- (2092764894,1601120196 1655838864)

-- λ> next (snd (next newSg))
-- (1390461064,1346387765 2103410263)


-- A few examples with random
-- Since random can generate values of different types, we need to specify the type to use.
-- random newSg :: Random a => (a, StdGen)

-- λ> random newSg :: (Int, StdGen)
-- (138890298504988632,439883729 1872071452)

-- λ> random newSg :: (Double, StdGen)
-- (0.41992072972993366,439883729 1872071452)


-- What if we want a number within a range?
-- class Random a where
--   randomR :: RandomGen g => (a, a) -> g -> (a, g)
--   default randomR :: (RandomGen g, UniformRange a) => (a, a) -> g -> (a, g)
--   randomR r g = runStateGen g (uniformRM r)

-- λ> r = randomR (0, 3) newSg
-- λ> :t r
-- r :: (Random a, Num a) => (a, StdGen)
-- λ> r :: (Int, StdGen)
-- (1,1601120196 1655838864)
-- λ> r :: (Double, StdGen)
-- (1.259762189189801,439883729 1872071452)
