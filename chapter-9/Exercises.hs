module Exercises where

import Data.Char

filterUpper xs = filter isUpper xs

capitalizeFirstChar :: [Char] -> [Char]
capitalizeFirstChar [] = []
capitalizeFirstChar (x:xs) = (toUpper x): capitalizeFirstChar xs

capitalizeHead :: [Char] -> Char
capitalizeHead xs = r xs
        where
            r = toUpper . head

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) =
        if(x)
         then True
         else myOr xs


myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) =
        if(f x)
        then True
        else myAny f xs


myElem :: Eq a => (a -> a -> Bool) -> a -> [a] -> Bool
myElem f a [] = False
myElem f a (x:xs) =
    if(f a x)
    then True
    else myElem f a xs

myElemUsingAny :: Eq a => a -> [a] -> Bool
myElemUsingAny = myElem (==)
