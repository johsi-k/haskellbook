-- The State newtype

newtype State s a =
  State { runState :: s -> (a, s) }

newtype Reader r a =
  Reader { runReader :: r -> a }

-- A newtype has the same underlying representation as the type it wraps.
-- Therefore the function contained in the newtype must be isomorphic to the type it wraps

-- The following is an isomorphism:
type Iso a b = (a -> b, b -> a)

newtype Sum a = Sum { getSum :: a }
-- Sum :: a -> Sum a
-- getSum :: Sum a -> a

sumIsIsomorphicWithItsContents :: Iso a (Sum a)
sumIsIsomorphicWithItsContents = (Sum, getSum)

-- Not an isomorphism
-- (a -> Maybe b, b -> Maybe a)
-- [a] -> a, a -> [a]

-- random looks a lot like State:
-- random :: (Random a)
--        => StdGen -> (a, StdGen)
-- State { runState
--        :: s      -> (a, s) }

-- randomR, once partially applied, also resembles State
-- randomR :: (...) => (a, a) -> g -> (a, g)
-- State { runState ::           s -> (a, s) }
