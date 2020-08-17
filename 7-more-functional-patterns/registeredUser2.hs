module RegisteredUser where

newtype Username =
  Username String

newtype AccountNumber =
  AccountNumber Integer

-- User is a sum with two constructors: UnregisteredUser and RegisteredUser
-- we can use pattern matching to dispatch our function differently,
-- depending on the value we get

-- RegisteredUser is a product of 2 newtypes:
-- Username and AccountNumber
data User =
    UnregisteredUser
  | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser =
  putStrLn "UnregisteredUser"
printUser (RegisteredUser
          (Username name)
          (AccountNumber acctNum)) =
  putStrLn $ name ++ " " ++ show acctNum

-- RegisteredUser :: Username -> AccountNumber -> User
-- a function that CONSTRUCTS a User out of the args Username and AccountNumber
-- hence a data constructor
