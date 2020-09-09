-- instance Enum Bool where ...
-- fromEnum False = 0
-- fromEnum True = 1
eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool True False = []
eftBool bool _     = [bool]

-- instance Enum ordering where ...
-- fromEnum LT = 0
-- fromEnum EQ = 1
-- fromEnum GT = 2
eftOrd :: Ordering
       -> Ordering
       -> [Ordering]
eftOrd LT GT = [LT, EQ, GT]
eftOrd GT LT = []
eftOrd LT EQ = [LT, EQ]
eftOrd EQ LT = []
eftOrd EQ GT = [EQ, GT]
eftOrd GT EQ = []
eftOrd ord _ = [ord]

eftInt :: Int -> Int -> [Int]
eftInt i1 i2
  | i1 == i2 = [i1]
  | i1 < i2 = i1 : eftInt (succ i1) i2
  | i1 > i2 = []
  | otherwise = undefined

eftChar :: Char -> Char -> String
eftChar c1 c2
  | c1 == c2 = [c1]
  | c1 < c2 = c1 : eftChar (succ c1) c2
  | c1 > c2 = []
  | otherwise = undefined
