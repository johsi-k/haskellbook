data Identity a =
  Identity a

-- this wouldn't work because a has to have an instance of Eq
-- in order for v and v' (both of type a) to be compared:
-- instance Eq (Identity a) where
--   (==) (Identity v) (Identity v') = v == v'

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

data NoEqInst = NoEqInst
