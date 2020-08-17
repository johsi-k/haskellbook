-- write the Eq instance for the datatype provided

-- 1.
data TisAnInteger =
  TisAn Integer -- TisAn is a custom type with param of type Integer

instance Eq TisAnInteger where
  TisAn integer == TisAn integer' =
    integer == integer'

-- 2.
data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (Two integer1 integer2) == (Two integer1' integer2') =
       integer1 == integer1'
    && integer2 == integer2'

-- 3.
data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt int) (TisAnInt int') =
    int == int'
  (==) (TisAnInt _) (TisAString _) =
    False
  (==) (TisAString str) (TisAString str') =
    str == str'
  (==) (TisAString _) (TisAnInt _) =
    False

-- 4.
data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a1 a2) (Pair a1' a2') =
    a1 == a1' && a2 == a2'

-- 5.
data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') =
    a == a' && b == b'

-- 6.
data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) (ThisOne _) (ThatOne _) = False
  (==) (ThatOne _) (ThisOne _) = False

-- 7.
data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello b) = a == b
  (==) (Goodbye a) (Goodbye b) = a == b
  (==) (Hello _) (Goodbye _) = False
  (==) (Goodbye _) (Hello _) = False
