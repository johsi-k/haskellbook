module Arith4 where

-- id :: a -> a
-- id x = x

-- read :: Read a => String -> a
-- show :: Show a => a -> String

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

main :: IO ()
main = do
  print (roundTrip 4)
  print (id 4)

-- 5.
-- write a pointfree version of roundTrip
roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

-- 6.
-- Change the type of roundTrip to (Show a, Read a) => a -> b
-- How might we tell GHC which instance of Read to dispatch against the String now?
-- print (roundTrip 4 :: Int)
