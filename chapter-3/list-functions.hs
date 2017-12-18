module ListFunctions where

myHeadFunc :: [Char] -> [Char]    
myHeadFunc x = x ++ "!" 

charAt5thPost :: [Char] -> Char
charAt5thPost x = x !! 5

drop9Chars :: [Char] -> [Char]
drop9Chars x = drop 9 x

thirdCharInInput :: [Char] -> Char
thirdCharInInput x = x !! 2

nthCharInString :: Int -> Char
nthCharInString n = "Curry is awesome" !! n

rvrs :: [Char] -> [Char]

rvrs = "Abcdef"