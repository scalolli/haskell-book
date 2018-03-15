module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "un matched digit"

digits :: Int -> [Int]
digits n = go n []
    where go num acc
            | num < 10 = num : acc
            | otherwise = go quotient accumulator
            where
              quotient = num `div` 10
              remainder = num `mod` 10
              accumulator = remainder : acc

wordNumber :: Int -> String
wordNumber n = concatString
    where digitsInWords = map digitToWord (digits n)
          interspersed = intersperse "-" digitsInWords
          concatString = concat interspersed



