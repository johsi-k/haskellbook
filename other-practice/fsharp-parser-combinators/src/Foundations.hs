{-# LANGUAGE InstanceSigs #-}

module Foundations where

import Control.Applicative
import Data.Functor.Alt (Alt((<!>)))

-- Implementation 1. Parsing a hard-coded character

-- Stream is empty : return False and empty string
-- First char in stream is A : return True and remaining stream of chars
-- First char in stream is not A : return False and unchanged original stream of chars
aParser :: String -> (Bool, String)
aParser ""       = (False, "")
aParser ('A':cs) = (True, cs)
aParser cs       = (False, cs)

-- Implementation 2. Parsing a specified character

-- Let's refactor so that we can pass in the character we want to match
-- Instead of returning True/False, we'll return a message indicating what happened

pChar' :: (Char, String) -> (String, String)
pChar' (_, "") = ("No more input", "")
pChar' (c', ccs@(c:cs))
  | c == c'   = ("Found " ++ [c], cs)
  | otherwise = ("Expecting '" ++ [c'] ++ "'. Got '" ++ [c] ++ "'", ccs)


-- Implementation 3. Returning a Success/Failure

-- We want to be able to tell the difference between a successful match and a failure, and returning a stringly-typed message is not very helpful, so we'll define a special "choice" type to indicate the difference.

data Result a =
    Success a
  | Failure String
  deriving (Show, Eq)

pCharResult :: (Char, String) -> Result (Char, String)
pCharResult (_, "") = Failure "No more input"
pCharResult (c', c:cs)
  | c == c'   = Success (c, cs)
  | otherwise = Failure $ "Expecting '" ++ [c'] ++ "'. Got '" ++ [c] ++ "'"


-- Implementation 4. Switching to a curried implementation

-- automatic currying
pCharCurried :: Char -> String -> Result (Char, String)
pCharCurried _ "" = Failure "No more input"
pCharCurried c' (c:cs)
  | c == c'   = Success (c, cs)
  | otherwise = Failure $ "Expecting '" ++ [c'] ++ "'. Got '" ++ [c] ++ "'"

-- explicit currying with inner function
pCharCurried' :: Char -> (String -> Result (Char, String))
pCharCurried' c' =
  let innerFn "" = Failure "No more input"
      innerFn (c:cs)
        | c == c'   = Success (c, cs)
        | otherwise = Failure $ "Expecting '" ++ [c'] ++ "'. Got '" ++ [c] ++ "'"
  in innerFn


-- Implementation 5. Encapsulating the parsing function in a type

-- parseA (below) has a function type String -> Result (Char, String)
-- We can encapsulate that in a Parser type

data Parser a = Parser (String -> Result (a, String))

pChar :: Char -> Parser Char
pChar c' =
  let innerFn "" = Failure "No more input"
      innerFn (c:cs)
        | c == c'   = Success (c, cs)
        | otherwise = Failure $ "Expecting '" ++ [c'] ++ "'. Got '" ++ [c] ++ "'"
  in Parser innerFn

run :: Parser a -> String -> Result (a, String)
run parser =
  let (Parser innerFn) = parser
  in innerFn


-- andThen combinator
-- works for any two parsers, and they can be of different types
andThen :: Parser a -> Parser b -> Parser (a, b)
-- andThen parser1 parser2 =
--   let innerFn input =
--         case run parser1 input of
--            Failure err -> Failure err
--            Success (value1, remaining1) ->
--              case run parser2 remaining1 of
--                Failure err -> Failure err
--                Success (value2, remaining2) -> Success ((value1, value2), remaining2)
--   in Parser innerFn

andThen parser1 parser2 =
  let innerFn inp = do
        (val1, rem1) <- run parser1 inp
        (val2, rem2) <- run parser2 rem1
        Success ((val1, val2), rem2)
  in Parser innerFn

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Applicative Result where
  pure :: a -> Result a
  pure = Success

  (<*>) :: Result (a -> b) -> Result a -> Result b
  (<*>) (Failure e) _ = Failure e
  (<*>) (Success f) r = fmap f r

instance Monad Result where
  (>>=) :: Result a -> (a -> Result b) -> Result b
  (>>=) (Failure e) _ = Failure e
  (>>=) (Success a) f = f a


-- orElse combinator
orElse :: Parser a -> Parser a -> Parser a
orElse parser1 parser2 =
  -- let innerFn inp =
  --       let res1 = run parser1 inp
  --       in case res1 of
  --         Success _ -> res1
  --         Failure _ -> run parser2 inp
  let innerFn inp = run parser1 inp <!> run parser2 inp
  in Parser innerFn

instance Alt Result where
  s@Success{} <!> _       = s
  _ <!> s@Success{}       = s
  Failure m <!> Failure n = Failure (m ++ "; " ++ n)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f pa =
    let innerFn inp = do
          (a, rest) <- run pa inp
          return (f a, rest)
    in Parser innerFn

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser (\s -> Success (a, s))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) pab pa =
    -- let innerFn inp = do
    --       (a, rem1) <- run pa inp
    --       (f, rem2) <- run pab rem1
    --       return (f a, rem2)
    -- in Parser innerFn
    fmap (\(f, x) -> f x) (pab `andThen` pa)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser Failure

  (<|>) :: Parser a -> Parser a -> Parser a
  pa <|> pa' =
    let innerFn inp = run pa inp <!> run pa' inp
    in Parser innerFn

-- choosing from a list of parsers: 'choice' and 'anyOf'
choice :: [Parser a] -> Parser a
choice = foldr orElse empty

anyOf :: String -> Parser Char
anyOf = choice . fmap pChar


inputABC :: String
inputABC = "ABC"

inputZBC :: String
inputZBC = "ZBC"

-- foundations :: IO ()
-- foundations = do
  -- good input
  -- print $ aParser inputABC

  -- bad input
  -- print $ aParser inputZBC

  -- print $ pChar ('A', inputABC)
  -- print $ pChar ('A', inputZBC)

  -- print $ pCharResult ('A', inputABC)
  -- print $ pCharResult ('A', inputZBC)

  -- print $ pCharCurried 'A' inputABC
  -- print $ pCharCurried 'A' inputZBC

  -- partial application with curried implementation
  -- let parseACurried = pCharCurried 'A'
  -- print $ parseACurried inputABC
  -- print $ parseACurried inputZBC
