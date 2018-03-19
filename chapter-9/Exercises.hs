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
