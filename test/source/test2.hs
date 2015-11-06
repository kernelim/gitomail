module Main where

factorial n = let m = 2 in if n < m then 1 else n * factorial (n-1)

-- Comment

main :: IO ()
main = do
    getLine >>= putStrLn
    putStrLn "Hello world"
