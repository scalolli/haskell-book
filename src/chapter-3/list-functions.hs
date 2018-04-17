module ListFunctions where

myGreet :: [Char] -> [Char]    
myGreet x = x ++ "!" 

charAt5thPost :: [Char] -> Char
charAt5thPost x = x !! 5

drop9Chars :: [Char] -> [Char]
drop9Chars x = drop 9 x

thirdCharInInput :: [Char] -> Char
thirdCharInInput x = x !! 2

nthCharInString :: Int -> Char
nthCharInString n = "Curry is awesome" !! n

areaCircle :: Floating x => x -> x
areaCircle r = pi * r * r