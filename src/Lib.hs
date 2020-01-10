module Lib (
    helloWorld,
    getName,
    greet,
) where

helloWorld :: IO ()
helloWorld = do
    putStrLn "Hello world!"
    return ()

getName :: IO (String)
getName = do
    putStrLn "Hello! What is your name? "
    name <- getLine
    return name

greet :: String -> String
greet name = "Greetings, " ++ name ++ "!"
