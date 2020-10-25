import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid
import Lib ( List
           , ZipList'
           , Validation
           , Pair
           , Two
           , Three
           , Three'
           , Four
           , Four'
           )

main :: IO ()
main = do
  -- List Applicative Exercise
  quickBatch (applicative (undefined :: List (Int, Int, Int)))

  -- ZipList Applicative Exercise
  quickBatch (applicative (undefined :: ZipList' (Int, Int, Int)))

  -- Exercise: Variations on Either
  quickBatch (applicative (undefined :: Validation (Sum Int) (Int, Int, Int)))

  -- Chapter Exercises
  quickBatch (applicative (undefined :: Pair (Int, Int, Int)))
  quickBatch (applicative (undefined :: Two (Sum Int) (Int, Int, Int)))

  quickBatch (applicative (undefined :: Three (Sum Int) (Sum Int) (Int, Int, Int)))
  quickBatch (applicative (undefined :: Three' (Sum Int) (Int, Int, Int)))

  quickBatch (applicative (undefined :: Four (Sum Int) (Sum Int) (Sum Int) (Int, Int, Int)))
  quickBatch (applicative (undefined :: Four' (Sum Int) (Int, Int, Int)))
