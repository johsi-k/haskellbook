import Data.List

-- 1. Define the data type Form
data Form =
    C Bool
  | Not Form
  | Form `And` Form
  | Form `Or` Form
  | V String
  deriving (Eq, Ord, Show, Read)


-- 3. Simplify Constants
-- T ∧ T = T
-- T ∧ F = F
-- F ∧ T = F
-- F ∧ F = F

-- T ∨ T = T
-- T ∨ F = T
-- F ∨ T = T
-- F ∨ F = F

removeConst :: Form -> Form
removeConst (Not (C bool))              = C $ not bool
removeConst ((C True)  `And` f)         = f
removeConst ((C False) `And` _)         = C False
removeConst (f         `And` (C True))  = f
removeConst (_         `And` (C False)) = C False
removeConst ((C False) `Or` f)          = f
removeConst ((C True)  `Or` _)          = C True
removeConst (f         `Or` (C False))  = f
removeConst (_         `Or` (C True))   = C True
removeConst f                           = f


-- 4.
simplify :: Form -> Form
simplify (f1 `And` f2) = removeConst (simplify f1 `And` simplify f2)
simplify (f1 `Or`  f2) = removeConst (simplify f1 `Or` simplify f2)
simplify (Not f)       = removeConst (Not (simplify f))
simplify (C b)         = C b
simplify (V a)         = V a


-- 5. Negation normal form
-- ¬ (¬ p)   = p       -- double negation
-- ¬ (p ∨ q) = ¬p ∧ ¬q -- De Morgan's
-- ¬ (p ∧ q) = ¬p ∨ ¬q -- De Morgan's

nnf :: Form -> Form
nnf (Not (Not f))       = nnf f
nnf (Not (f1 `And` f2)) = nnf (Not f1) `Or` nnf (Not f2)
nnf (Not (f1 `Or` f2))  = nnf (Not f1) `And` nnf (Not f2)
nnf (f1 `And` f2)       = nnf f1 `And` nnf f2
nnf (f1 `Or` f2)        = nnf f1 `Or` nnf f2
nnf (Not f)             = Not (nnf f)
nnf (C b)               = C b
nnf (V a)               = V a


-- 6. Conjuctive normal form
-- p ∨ (q ∧ r) ≡ (p ∨ q) ∧ (p ∨ r)
-- (q ∧ r) ∨ p ≡ (q ∨ p) ∧ (r ∨ p)

distOr :: Form -> Form -> Form
distOr p (q `And` r) = distOr p q `And` distOr p r
distOr (p `And` q) r = distOr p r `And` distOr q r
distOr p q           = p `Or` q

cnf :: Form -> Form
cnf (Not f)       = Not $ cnf f
cnf (C b)         = C b
cnf (V a)         = V a
cnf (f1 `Or` f2)  = distOr (cnf f1) (cnf f2)
cnf (f1 `And` f2) = cnf f1 `And` cnf f2

-- domain-specific pretty-printing
pp :: Form -> String
pp (Not a)   = "¬" ++ pp a
pp (And a b) = pp a ++ pp b
pp (Or a b)  = "(" ++ pp a ++ " + " ++ pp b ++ ")"
pp (V a)     = a
pp (C a)     = show a


-- 7. Extract free variables
-- nub :: Eq a => [a] -> [a]
fvList :: Form -> [String]
fvList (V s)         = [s]
fvList (C _)         = []
fvList (Not f)       = fvList f
fvList (f1 `And` f2) = nub $ fvList f1 ++ fvList f2
fvList (f1 `Or` f2)  = nub $ fvList f1 ++ fvList f2


-- 8. Traverse a formula and substitute a single variable with a value
subst :: Form -> (String, Bool) -> Form
subst (V s) (s', b)
      | s == s'               = C b
      | otherwise             = V s
subst (C b) (_, _)            = C b
subst (Not f) (s, b)          = Not $ subst f (s, b)
subst (f1 `And` f2) (s, b)    = subst f1 (s, b) `And` subst f2 (s, b)
subst (f1 `Or` f2) (s, b)     = subst f1 (s, b) `Or` subst f2 (s, b)


-- 9. Substitute all variables with their given values (valuation)
substAll :: Form -> [(String, Bool)] -> Form
substAll = foldl' subst


-- 10. Evaluate a formula under a valuation
evalSubst :: Form -> [(String, Bool)] -> Bool
evalSubst f subs = bool
  where (C bool) = simplify $ substAll f subs


-- 11. Enumerate all valuations
models :: Form -> [(String, Bool)] -> [String] -> [[(String, Bool)]]
models f vl []     | evalSubst f vl = [vl] | otherwise = []
models f vl (v:vs) = models f ((v, True):vl) vs ++ models f ((v, False):vl) vs


-- 12.
allModels :: Form -> [[(String, Bool)]]
allModels f = models f [] (fvList f)


-- 13. f is unsatisfiable if it is true for no valuation
unsatisfiable :: Form -> Bool
unsatisfiable = null . allModels


-- 14. f is valid iff ¬f is unsatisfiable
valid :: Form -> Bool
valid = unsatisfiable . Not
