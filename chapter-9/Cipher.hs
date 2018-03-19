module Cipher where

import Data.Char

myCipher :: [Char] -> [Char]
myCipher [] = []
myCipher (x:xs) = (shift x):(myCipher xs)

shift :: Char -> Char
shift x = chr $ ((ord x) + 3) `mod` 26

