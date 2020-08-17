data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
-- pattern matching
-- we can define a function by matching on a DATA constructor
-- and describing the behaviour that the function should have
-- based on which value it matches
changeMood Blah = Woot
changeMood _ = Blah -- underscore denotes a catch-all