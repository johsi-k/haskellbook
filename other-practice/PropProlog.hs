import PropForms (Form (..))
import Data.List

-- 15. Define the Rule, Goal and Prog data types
data Rule = Rl String [String] deriving Show
newtype Goal = Gl [String] deriving Show
data Prog = Pr [Rule] Goal deriving Show

-- Sample programs:

-- h.
-- m :- h
-- ?- m
mortalSocrates :: Prog
mortalSocrates =
  Pr [ Rl "h" []
     , Rl "m" ["h"] ]
  (Gl ["m"])

-- h.
-- h :- m
-- ?- m
immortalSocrates :: Prog
immortalSocrates =
  Pr [ Rl "h" []
     , Rl "h" ["m"] ]
  (Gl ["m"])

-- a.
-- d :- b, c.
-- d :- a, c.
-- c :- a.
-- ?- d, c.
abcdProg :: Prog
abcdProg =
  Pr [ Rl "a" []
     , Rl "d" ["b", "c"]
     , Rl "d" ["a", "c"]
     , Rl "c" ["a"] ]
  (Gl ["d", "c"])


-- 16. Translate rules, goals and programs to the formulas outlined above
-- A → B = ¬A ∨ B
implies :: Form -> Form -> Form
implies f1 f2 = Not f1 `Or` f2

conj :: [Form] -> Form
conj = foldl' And (C True)

ruleToForm :: Rule -> Form
ruleToForm (Rl h ps) = implies (conj (map V ps)) (V h)

goalToForm :: Goal -> Form
goalToForm (Gl gs) = conj (map V gs)

progToForm :: Prog -> Form
progToForm (Pr rs g) =
  implies (conj (map ruleToForm rs)) (goalToForm g)

-- h :- p1, ..., pn
-- p₁ ∧ ... ∧ pn → h


-- 17.
-- Check whether a number is divisible by 3 in the list [1, 4, 5, 6]
anyDivBy3 :: [Integer] -> Bool
anyDivBy3 = any (\x -> x `mod` 3 == 0)

-- Check if all the numbers in this list are divisible by 3
allDivBy3 :: [Integer] -> Bool
allDivBy3 = all (\x -> x `mod` 3 == 0)


-- 18.
-- Use list comprehension to compute the squares of the negative numbers of [1, -3, 4, -5] (the result should be [9,25]).
squaresNeg :: [Integer] -> [Integer]
squaresNeg xs = [x^2 | x <- xs, x < 0]

-- Use list comprehension to find the rules that have a given head
rulesGivenHead :: [Rule] -> String -> [Rule]
rulesGivenHead rules h = [r | r@(Rl h' _) <- rules, h' == h]


-- 19.
-- Solve a goal by solving all propositions of the goal
solveGoal :: [Rule] -> [String] -> Bool
solveGoal = all . solveProp

-- Solve a proposition by finding a rule whose head matches the proposition, and then solving the goal consisting of propositions p1..pn
solveProp :: [Rule] -> String -> Bool
solveProp rls p = any (\(Rl _ ps) -> solveGoal rls ps) (rulesGivenHead rls p)


-- 20. Fetch the rule set and goal (list of propositions) out of a program and start solveGoal on them
runProg :: Prog -> Bool
runProg (Pr rls (Gl ps)) = solveGoal rls ps

nonterminating :: Prog
nonterminating =
  Pr [ Rl "h" []
     , Rl "m" ["m", "h"]
     , Rl "m" ["h"] ]
  (Gl ["m"])

terminating :: Prog
terminating =
  Pr [ Rl "h" []
     , Rl "m" ["h"] ]
  (Gl ["m"])
