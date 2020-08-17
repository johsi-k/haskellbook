data Trivial =
  Trivial'

-- writing an instance of Eq for Trivial
instance Eq Trivial where
  Trivial' == Trivial' = True
