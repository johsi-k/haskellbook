module Print2 where

main :: IO ()
main = do -- do allows for sequencing actions
  putStrLn "Count to four for me:"
  putStr "one, two"
  putStr ", three, and"
  putStrLn " four!"