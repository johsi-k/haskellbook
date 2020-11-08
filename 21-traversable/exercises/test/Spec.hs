import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

import Lib ( Identity
           , Constant
           , Optional
           , List
           , Three
           , Pair
           , Big
           , Bigger
           , S
           , Tree
           )

main :: IO ()
main = do
  putStr "Identity"
  quickBatch (traversable (undefined :: Identity (Int, Int, [Int])))

  putStr "\nConstant"
  quickBatch (traversable (undefined :: Constant Int (Int, Int, [Int])))

  putStr "\nOptional"
  quickBatch (traversable (undefined :: Optional (Int, Int, [Int])))

  putStr "\nList"
  quickBatch (traversable (undefined :: List (Int, Int, [Int])))

  putStr "\nThree"
  quickBatch (traversable (undefined :: Three Int Int (Int, Int, [Int])))

  putStr "\nPair"
  quickBatch (traversable (undefined :: Pair Int (Int, Int, [Int])))

  putStr "\nBig"
  quickBatch (traversable (undefined :: Big Int (Int, Int, [Int])))

  putStr "\nBigger"
  quickBatch (traversable (undefined :: Bigger Int (Int, Int, [Int])))

  putStr "\nS"
  quickBatch (traversable (undefined :: S Maybe (Int, Int, [Int])))

  putStr "\nTree"
  quickBatch (traversable (undefined :: Tree (Int, Int, [Int])))
