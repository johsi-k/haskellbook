-- putStr :: String -> IO ()
-- putStrLn :: String -> IO ()
-- these work only for arguments of type String

-- for non-String args, we can use show to first convert the arg to a String
-- show :: Show a => a -> String

-- print is a composition of show and putStrLn
-- print :: Show a => a -> IO ()
-- print a = putStrLn (show a)
