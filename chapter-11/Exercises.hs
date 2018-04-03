module Exercises where

    doubleUp :: [a] -> [a]
    doubleUp [] = []
    doubleUp xs@(x:_) = x:xs

    isSubseqOf :: (Eq a)
                => [a]
                -> [a]
                -> Bool   
    isSubseqOf [] _ = True
    isSubseqOf _ [] = False                 
    isSubseqOf l@(y:ys) (x:xs) = 
        if (y == x) 
            then isSubseqOf ys xs
             else isSubseqOf l xs
