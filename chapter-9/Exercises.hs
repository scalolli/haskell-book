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


myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x : xs) = (f x) ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap (\x -> x) xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "Minimum of empty list"
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) = findElemByComparisonFunction f (>= GT) xs x

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "Minimum of empty list"
myMinimumBy _ [x] = x
myMinimumBy f (x :xs) = findElemByComparisonFunction f (<= LT) xs x

findElemByComparisonFunction :: (a -> a -> Ordering) -> (Ordering -> Bool) -> [a] -> a -> a                        
findElemByComparisonFunction f g xs acc = 
    case xs of
        [] -> acc
        (y:ys) -> findElemByComparisonFunction f g ys minValue
            where
                minValue = if (g $ f y acc) then y else acc