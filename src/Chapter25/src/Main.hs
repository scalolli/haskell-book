module Main where

  main :: IO ()
  main = do
    line <- getLine
    putStrLn ("Hello, your line is " ++ line)
    putStrLn "Welcome to Chapter25 !"
