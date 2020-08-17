-- 1.
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = aToB a == b

-- 2.
-- use some arithmetic operation to combine the values of type 'b'
arith :: Num b
      => (a -> b)
      -> Integer
      -> a
      -> b
arith aToB i a = fromInteger i + aToB a

result = arith (\x -> let _ = x :: Char in 8 :: RealFrac a => a) 7 'a'


-- Chapter definitions on typeclass inheritance
newtype Nada =
  Nada Double deriving (Eq, Show)

instance Num Nada where
  (+) (Nada double) (Nada double') = Nada (double + double')
  (*) (Nada double) (Nada double') = Nada (double * double')
  negate (Nada double) = Nada (negate double)
  abs (Nada double) = Nada (abs double)
  signum (Nada double) = Nada (signum double)
  fromInteger _ = Nada 0

instance Fractional Nada where
  (Nada x) / (Nada y) = Nada (x / y)
  recip (Nada n) = Nada (recip n)
  fromRational r = Nada (fromRational r)
