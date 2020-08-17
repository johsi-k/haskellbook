-- we can use let expressions to declare and bind variables

bindExp :: Integer -> String
bindExp x =
  let y = 5 in
    "the integer was: " ++ show x
    ++ " and y was: " ++ show y
 -- y is only in scope inside the let expression


-- bindExp' :: Integer -> String
-- bindExp' x =
--   let z = y + x in
--     let y = 5 in
--       "the integer was: "
--       ++ show x ++ " and y was: "
--       ++ show y ++ " and z was: "
--       ++ show z
-- x is in scope since the function argument is visible anywhere in the function
-- y is not yet in scope since it's bound in the expression that let z wraps


bindExp' :: Integer -> String
bindExp' x =
  let y = 5;
      z = y + x in
      "the integer was: "
      ++ show x ++ " and y was: "
      ++ show y ++ " and z was: "
      ++ show z

-- the reference to x from arg x is shadowed by let x
bindExpShadow :: Integer -> String
bindExpShadow x =
  let x = 10; y = 5 in
    "the integer was: " ++ show x
    ++ " and y was: " ++ show y

-- > bindExpShadow 9001
-- > "the integer was: 10 and y was: 5"
-- lexical scoping: resolving the value for a named entity depends
-- on location in code and lexical context
-- the lexically innermost binding for a variable of a particular name takes precedence
