import qualified Data.Map as M
import Data.Maybe

type Morse = String

letterToMorse :: M.Map Char Morse
letterToMorse = M.fromList [
    ('a', ".-")
  , ('b', "-...")
  , ('c', "-.-.")
  , ('d', "-..")
  , ('e', ".")

  , ('f', "..-.")
  , ('g', "--.")
  , ('h', "....")
  , ('i', "..")
  , ('j', ".---")
  , ('k', "-.-")

  , ('l', ".-..")
  , ('m', "--")
  , ('n', "-.")
  , ('o', "---")
  , ('p', ".--.")
  , ('q', "--.-")

  , ('r', ".-.")
  , ('s', "...")
  , ('t', "-")
  , ('u', "..-")
  , ('v', "...-")
  , ('w', ".--")

  , ('x', "-..-")
  , ('y', "-.--")
  , ('z', "--..")

  , ('1', ".----")
  , ('2', "..---")
  , ('3', "...--")
  , ('4', "....-")
  , ('5', ".....")
  , ('6', "-....")
  , ('7', "--...")
  , ('8', "---..")
  , ('9', "----.")
  , ('0', "-----")
  ]

-- k = Morse, a = Char
-- M.foldrWithKey :: (Char -> Morse -> M.Map Morse Char -> M.Map Morse Char)
--                -> M.Map Morse Char -- empty
--                -> M.Map Char Morse -- letterToMorse
--                -> M.Map Morse Char

morseToLetter :: M.Map Morse Char
morseToLetter = M.foldrWithKey (flip M.insert) M.empty letterToMorse

-- M.lookup :: Morse -> M.Map Morse Char -> Maybe Char
morseToChar :: Morse -> Maybe Char
morseToChar m = M.lookup m morseToLetter

charToMorse :: Char -> Maybe Morse
charToMorse c = M.lookup c letterToMorse

stringToMorse :: String -> Maybe [Morse]
-- stringToMorse s = sequence $ fmap charToMorse s
stringToMorse = traverse charToMorse


-- Character conversion is wrapped in a Maybe due to the possibility of getting characters in a string that aren't translatable into Morse code (or in the opposite direction, aren't Morse characters).

-- λ> morseToChar "gobbledegook"
-- Nothing

-- λ> morseToChar "-.-."
-- Just 'c'

-- We can use fromMaybe to remove the Maybe layer
-- fromMaybe :: a -> Maybe a -> a
-- *Main Data.Maybe> fromMaybe ' ' (morseToChar "-.-.")
-- 'c'

-- *Main> stringToMorse "chris"
-- Just ["-.-.","....",".-.","..","..."]

-- *Main Data.Maybe> fromMaybe [] (stringToMorse "chris")
-- ["-.-.","....",".-.","..","..."]


-- Defining a helper
morse :: String -> [Morse]
morse s = fromMaybe [] (stringToMorse s)

-- fmapping morseToChar would give us a list of Maybe values:

-- λ> fmap morseToChar (morse "chris")
-- [Just 'c',Just 'h',Just 'r',Just 'i',Just 's']

-- To get a Maybe String we need sequence
-- To use sequence over a list of Maybe (or other monadic) values, we need to compose it with fmap
-- λ> sequence $ fmap morseToChar (morse "chris")
-- Just "chris"
-- λ> sequence . fmap morseToChar $ (morse "chris")
-- Just "chris"
-- λ> ((sequence .) . fmap) morseToChar (morse "chris")
-- Just "chris"
