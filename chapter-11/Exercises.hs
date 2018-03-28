module Exercises where

    doubleUp :: [a] -> [a]
    doubleUp [] = []
    doubleUp xs@(x:_) = x:xs

    isSubsetOf :: (Eq a)
                => [a]
                -> [a]
                -> Bool
    isSubsetOf = undefined