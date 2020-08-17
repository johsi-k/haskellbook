data Segment = Head | Thorax | Abdomen deriving Ord

instance Eq Segment where
  (==) Head Head       = True
  (==) Thorax Thorax   = True
  (==) Abdomen Abdomen = True
  (==) _ _             = False

data Capoo = Cat | Bug [Segment]

instance Eq Capoo where
  Cat == Cat = True
  Cat == Bug _ = False
  Bug _ == Cat = False
  -- Bug [] == Bug _ = False
  -- Bug _ == Bug [] = False
  -- Bug [s] == Bug [s'] = s == s'
  -- Bug [_] == Bug (_:_:_) = False
  -- Bug (_:_:_) == Bug (_:_) = False

  -- lists are just values, no need for destructuring
  -- lists implement Eq!
  Bug segments == Bug segments' = segments == segments'

instance Ord Capoo where
  compare Cat Cat = EQ
  compare (Bug s) (Bug s') = compare s s'
  compare Cat (Bug []) = GT
  compare (Bug []) Cat  = LT
  compare Cat (Bug [_]) = EQ
  compare (Bug [_]) Cat  = EQ
  compare Cat (Bug (_:_)) = LT
  compare (Bug (_:_)) Cat = GT
