module BuildingALib where

import Foundations

-- 1. fmap - Transforming the contents of a parser
parseDigit :: Parser Char
parseDigit = anyOf ['0'..'9']

-- parsing 3 digits as a nested tuple
parseThreeDigits :: Parser ((Char, Char), Char)
parseThreeDigits =
  parseDigit `andThen` parseDigit `andThen` parseDigit

-- see Foundations for Functor instance

parseThreeDigitsAsStr :: Parser String
parseThreeDigitsAsStr =
  fmap (\((c1, c2), c3) -> [c1, c2, c3]) parseThreeDigits

parseThreeDigitsAsInt :: Parser Int
parseThreeDigitsAsInt = fmap read parseThreeDigitsAsStr


-- 2. ap and pure - lifting functions to the world of Parsers
-- see Foundations for Applicative instance


-- 3. sequence - transforming a list of Parsers into a single Parser
parsers :: [Parser Char]
parsers = [pChar 'A', pChar 'B', pChar 'C']

-- sequenceA :: [Parser a] -> Parser [a]
combined :: Parser String
combined = sequenceA parsers

-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
-- f ~ Parser, t ~ []
-- traverse :: (Char -> Parser Char) -> [Char] -> Parser [Char]
pString :: String -> Parser String
pString = traverse pChar


-- 4. many and many1 - matching a parser multiple times

parseZeroOrMore :: Parser a -> String -> ([a], String)
parseZeroOrMore p inp =
  case run p inp of
    -- if parse fails, return empty list
    Failure _ -> ([], inp)
    -- if parse succeeds, recurse to get subsequent values
    Success (fstVal, inpAftFstP) ->
      let (subVals, remInp) = parseZeroOrMore p inpAftFstP
          vals = fstVal : subVals
      in (vals, remInp)

many :: Parser a -> Parser [a]
many p =
  let innerFn inp = Success (parseZeroOrMore p inp)
  in Parser innerFn

many1 :: Parser a -> Parser [a]
many1 p =
  let innerFn inp =
        case run p inp of
          Failure err -> Failure err
          Success (fstVal, inpAftFstP) ->
            let (subVals, remInp) = parseZeroOrMore p inpAftFstP
                vals = fstVal : subVals
            in Success (vals, remInp)
  in Parser innerFn

-- does not handle negative integers
pInt' :: Parser Integer
pInt' =
  let digit = anyOf ['0'..'9']
      digits = many1 digit
  in fmap read digits


-- 5. opt - matching a parser zero or once
opt :: Parser a -> Parser (Maybe a)
opt p =
  let some = Just <$> p
      none = pure Nothing
  in some `orElse` none

pInt :: Parser Integer
pInt =
  let
    resultToInt (sign, intStr) =
      case sign of
        Just _ -> negate (read intStr)
        Nothing -> read intStr
    digit = anyOf ['0'..'9']
    digits = many1 digit
  in resultToInt <$> opt (pChar '-') `andThen` digits


-- 6. Throwing results away
keepLeft :: Parser a -> Parser b -> Parser a
keepLeft p1 p2 =
  fmap fst (p1 `andThen` p2)

keepRight :: Parser a -> Parser b -> Parser b
keepRight p1 p2 =
  fmap snd (p1 `andThen` p2)


-- looking for a parser between delimiters such as quotes/brackets
between :: Parser a -> Parser b -> Parser c -> Parser b
between p1 p2 p3 = p1 `keepRight` p2 `keepLeft` p3


-- 7. Parsing lists with separators
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep =
  let sepThenP = sep `keepRight` p
  in fmap (uncurry (:)) (p `andThen` many sepThenP)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep =
  sepBy1 p sep `orElse` pure []


buildingALib :: IO ()
buildingALib = do
  print $ run parseThreeDigits "123A"
  print $ run parseThreeDigitsAsStr "123A"
  print $ run parseThreeDigitsAsInt "123A"

  print $ run combined "ABCD"
