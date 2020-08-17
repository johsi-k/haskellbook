funcZ x =
  case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"

-- checks whether something is a palindrome
pal xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"

-- or
pal' xs =
  case y of
    True -> "yes"
    False -> "no"
  where y = xs == reverse xs

-- greetIfCool
greetIfCool :: String -> IO ()
greetIfCool coolness =
  case cool of
    True ->
      putStrLn "eyyyyy. What's shakin'?"
    False ->
      putStrLn "pshhhh."
  where cool =
          coolness == "downright frosty yo"
