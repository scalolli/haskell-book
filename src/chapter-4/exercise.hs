module Exercises where

myLength :: [a] -> Int
myLength l = length l

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x