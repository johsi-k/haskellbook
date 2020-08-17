-- 1.
-- functionC x y = if (x > y) then x else y
functionC x y =
  case x > y of
    True -> x
    False -> y

-- 2.
-- ifEvenadd 2 n = if even n then (n+2) else n
ifEvenadd2 n =
  case even n of
    True -> n + 2
    False -> n

-- 3.
-- the following compares x to zero and returns
-- an indicator for whether x is +ve or -ve
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

