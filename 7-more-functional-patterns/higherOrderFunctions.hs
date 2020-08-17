-- higher-order functions
-- flip :: (a -> b -> c) -> b -> a -> c
-- when we want to express a func arg within a func type,
-- we must use parentheses to nest it

-- we could implement flip in these ways:
-- flip f x y = f y x
-- flip f = \x y -> f y x

-- to verify that the arguments are indeed flipped
f = flip ((\x y -> (x, y)) :: Int -> String -> (Int, String))
-- f :: String -> Int -> (Int, String)

returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

-- this is equivalent
returnLast' :: a -> (b -> (c -> (d -> d)))
returnLast' _ _ _ d = d

-- this breaks
-- returnBroke :: (((a -> b) -> c) -> d) -> d
-- returnBroke _ _ _ d = d

data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++
             " is the boss of " ++
             show e'

-- subvert hierarchy with a modified comparison function
codersRuleCEOsDrool :: Employee
                    -> Employee
                    -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _  = GT
codersRuleCEOsDrool _ Coder  = LT
codersRuleCEOsDrool e e' =
  compare e e'

employeeRank :: (  Employee
                -> Employee
                -> Ordering )
             -> Employee
             -> Employee
             -> IO ()
employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee\
                   \ is the boss"
    LT -> (flip reportBoss) e e'
