module Exercises where

    import Data.Char

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

    
    capitalizeWords :: String -> [(String, String)]
    capitalizeWords xs = map go (splitWords xs [] [])
            where go l@(_:_) = (l, capitalizeWord l) 

    splitWords :: String -> String -> [String] -> [String]
    splitWords [] word acc = acc ++ [word]
    splitWords (x:xs) word acc = 
        if(x == ' ')
        then
            splitWords xs [] (acc ++ [word])
        else 
            splitWords xs (word ++ [x]) acc

    capitalizeWord :: String -> String
    capitalizeWord [] = []
    capitalizeWord (x:xs) = [toUpper x] ++ xs

    alphabets = ['a'..'z'] ++ ['A'..'Z']

    splitPara :: String -> Bool -> String -> String
    splitPara [] _ acc = acc
    splitPara (x:xs) shouldCapitalize acc = 
        if(x == '.') 
            then 
                splitPara xs True (acc ++ [x])
            else if (x `elem` alphabets && shouldCapitalize)
                then splitPara xs False (acc ++ [toUpper x])
                else splitPara xs shouldCapitalize (acc ++ [x]) 