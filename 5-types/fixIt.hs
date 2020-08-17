module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

-- modify comparator to toggle songs
sing = if (x > y) then fstString x else sndString y
  where x = "Singin"
        y = "Somewhere"
