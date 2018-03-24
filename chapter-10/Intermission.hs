module Intermission where


fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Int -> Integer
fibsN x = fibs !! x

fibsTake20 :: [Integer]
fibsTake20 = take 20 fibs

fibsFilter :: [Integer]
fibsFilter = takeWhile (< 100) fibs

factorial :: [Integer]
factorial = scanl (*) 1 factorial

factorialN :: Int -> Integer
factorialN n = factorial !! n
