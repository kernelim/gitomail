module Main where

factorial n = if n < 2 then 1 else n * factorial (n-1)

-- Comment

main :: IO ()
main = do
    putStrLn "Hello world"

