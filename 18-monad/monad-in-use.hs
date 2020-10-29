-- Examples of Monad use

-- List

-- Specializing the types
-- (>>=) :: Monad m
--       => m a -> (a -> m b) -> m b
-- (>>=) :: [a] -> (a -> [b]) -> [b]

-- same as pure
-- return :: Monad m => a -> m a
-- return ::            a -> [a]

-- Example of List Monad in use

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs -- binds individual values from list input; x :: a
  if even x
    -- gives us a -> m b
    then [x*x, x*x]
    else [x*x]


-- Maybe Monad

-- Specializing the types

-- type M = Maybe
-- m ~ Maybe

-- (>>=) :: Monad m => m     a -> (a -> m     b) -> m     b
-- (>>=) ::            Maybe a -> (a -> Maybe b) -> Maybe b

-- same as pure
-- return :: Monad m => a -> m     a
-- return ::            a -> Maybe a


data Cow = Cow {
    name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing


-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
     then Nothing
     else Just c

mkSphericalCow :: String
               -> Int
               -> Int
               -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              weightCheck (Cow nammy agey weighty)


-- Cleaning it up with do syntax
mkSphericalCow' :: String
                -> Int
                -> Int
                -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)


-- Rewriting this with (>>=)
mkSphericalCow'' :: String
                 -> Int
                 -> Int
                 -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>=
  (\nammy ->
     noNegative age' >>=
     (\agey ->
       noNegative weight' >>=
     (\weighty ->
       weightCheck (Cow nammy agey weighty))))

-- noEmpty name' >>= (\nammy ..)
-- (>>=) :: m a -> (a -> m b) -> m b
-- (>>=) :: Maybe String -> (String -> Maybe Cow) -> Maybe Cow

-- noNegative age' >>= (\agey ..)
-- (>>=) :: m a -> (a -> m b) -> m b
-- (>>=) :: Maybe Int -> (Int -> Maybe Cow) -> Maybe Cow

-- noNegative weight' >>= (\weighty ..)
-- (>>=) :: m a -> (a -> m b) -> m b
-- (>>=) :: Maybe Int -> (Int -> Maybe Cow) -> Maybe Cow

-- weightCheck (Cow nammy agey weighty)
-- weightCheck :: Cow -> Maybe Cow
-- Cow :: String -> Int -> Int -> Cow


-- Why can't we do this with Applicative?

-- If we're doing this with do:
doSomething :: Monad m => m a -> m b -> m c -> m (a, b, c)
doSomething f g h = do
  a <- f
  b <- g
  c <- h
  pure (a, b, c)

-- We could rewrite it using Applicative
doSomethingApp :: Applicative f => f a -> f b -> f c -> f (a, b, c)
doSomethingApp x y z = (,,) <$> x <*> y <*> z

-- But if we have this:
doSomething' :: Monad m
             => n
             -> (n -> m a)
             -> (a -> m b)
             -> (b -> m c)
             -> m (a, b, c)
doSomething' n f g h = do
  a <- f n
  b <- g a
  c <- h b
  pure (a, b, c)


-- We could still rewrite this with Applicative
doSomethingElse :: Monad m => n -> (n -> m a) -> m a
doSomethingElse n f = do
  a <- f n
  pure a

doSomethingElse' :: Applicative f => t -> (t -> f a) -> f a
doSomethingElse' x f = f x


-- But how about this?
f1 :: Integer -> Maybe Integer
f1 0 = Nothing
f1 n = Just n

f2 :: Integer -> Maybe Integer
f2 i =
  if even i
  then Just (i + 1)
  else Nothing

f3 :: Integer -> Maybe String
f3 i = Just ("10191" ++ show i)

doSomethingg :: Integer -> Maybe (Integer, Integer, String)
doSomethingg n = do
  a <- f1 n
  b <- f2 a
  c <- f3 b
  pure (a, b, c)


-- Exploding a spherical cow
-- Let's see how binding over Maybe values works

-- instance Monad Maybe where
--   return x = Just x

--   (Just x) >>= k = k x
--   Nothing  >>= _ = Nothing

-- mkSphericalCow'' :: String
--                  -> Int
--                  -> Int
--                  -> Maybe Cow
-- mkSphericalCow'' name' age' weight' =
--   noEmpty name' >>=
--   \nammy ->
--     noNegative age' >>=
--     \agey ->
--       noNegative weight' >>=
--       \weighty ->
--         weightCheck (Cow nammy agey weighty)

-- Giving it some arguments,
-- mkSphericalCow'' "Bess" >>=
--   \nammy ->
--     noNegative 5 >>=
--     \agey ->
--       noNegative 499 >>=
--       \weighty ->
--         weightCheck (Cow nammy agey weighty)

-- noEmpty "Bess" = Just "Bess"
-- noEmpty "Bess" >>= (\nammy -> ..)

-- With reference to the Maybe instance of Monad,
-- (Just x) >>= k = k x
-- (Just "Bess") >>= (\nammy -> ..)
-- x = "Bess"; k = (\nammy -> ..)

-- nammy is bound to "Bess"
-- mkSphericalCow'' "Bess" 5 499 =
--   noEmpty "Bess" >==
--   \"Bess" ->
--     noNegative age' >>=
--     \agey ->
--       noNegative weight' >>=
--       \weighty ->
--         weightCheck (Cow "Bess" agey weighty)

-- For the age check, noNegative 5 = Just 5
-- but only 5 is passed on because >>= passes on only a from ma to the function it binds over the monadic value
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- (Just x) >>= k = k x
-- (Just 5) >>= (\agey ..)
-- x = 5; k = (\agey ..)

-- agey is bound to 5
-- mkSphericalCow'' "Bess" 5 499 =
--   noEmpty "Bess" >==
--   \"Bess" ->
--     noNegative 5 >>=
--     \5 ->
--       noNegative 499 >>=
--       \weighty ->
--         weightCheck (Cow "Bess" 5 weighty)

-- For the weight check, noNegative 499 = Just 499
-- weighty is now bound to 499
-- mkSphericalCow'' "Bess" 5 499 =
--   noEmpty "Bess" >==
--   \"Bess" ->
--     noNegative 5 >>=
--     \5 ->
--       noNegative 499 >>=
--       \499 ->
--         weightCheck (Cow "Bess" 5 499)

-- Finally,
-- weightCheck (Cow "Bess" 5 499) =
--   let 499 = weight (Cow "Bess" 5 499)
--       "Bess" = name (Cow "Bess" 5 499)
--   in if "Bess" == "Bess" && 499 > 499
--      then Nothing
--      else Just (Cow "Bess" 5 499)

-- 499 > 499 is False, so we get Just (Cow "Bess" 5 499)


-- What if we had failed?

-- λ> mkSphericalCow'' "" 5 499
-- Nothing

-- mkSphericalCow'' "" 5 499 =
--   noEmpty "" >==
--   \"" ->
--     noNegative 5 >>=
--     \5 ->
--       noNegative 499 >>=
--       \499 ->
--         weightCheck (Cow "Bess" 5 499)

-- noEmpty "" = Nothing

-- recalling the Monad Maybe instance again,
-- Nothing >>= _ = Nothing
-- Nothing >>= (\nammy -> ..)

-- The bind function drops the entire rest of the computation the moment any of the constituent functions produces a Nothing

-- λ> Nothing >>= undefined
-- Nothing
-- λ> Just 1 >>= undefined
-- *** Exception: Prelude.undefined


-- Either

-- Specializing the types

-- (>>=) :: Monad m => m        a -> (a -> m        b) -> m        b
-- (>>=) ::            Either e a -> (a -> Either e b) -> Either e b

-- same as pure
-- return :: Monad m => a -> m a

-- years ago
type Founded = Int

-- number of programmers
type Coders = Int

data SoftwareShop =
  Shop {
    founded :: Founded
  , programmers :: Coders
  } deriving (Eq, Show)

data FoundedError =
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0     = Left $ NegativeYears n
  | n > 500   = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0     = Left $ NegativeCoders n
  | n > 5000  = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded' <- validateFounded years
  programmers' <- validateCoders coders
  if programmers' > div founded' 10
    then Left $
           TooManyCodersForYears
           founded' programmers'
    else Right $ Shop founded' programmers'


-- Short Exercise: Either Monad
-- see exercises-without-quickcheck.hs
