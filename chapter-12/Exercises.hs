module Exercises where

    notThe :: String -> Maybe String
    notThe w =
        if (w == "the")
            then Nothing
            else Just w

    beginsWithVowel :: String -> Bool
    beginsWithVowel (w:ws) = w `elem` ['a', 'e', 'i', 'o', 'u']

    replaceThe :: String -> String
    replaceThe s = replaceThe' (words s)

    replaceThe' :: [String] -> String
    replaceThe' [] = ""
    replaceThe' (x:xs) = r ++ " " ++ (replaceThe' xs)
        where
            r = case (notThe x) of
                    Just w -> w
                    Nothing -> "a"


    countTheBeforeVowel :: String -> Integer
    countTheBeforeVowel s = countTheBeforeVowel' (words s) False 0

    countTheBeforeVowel' :: [String] -> Bool -> Integer -> Integer
    countTheBeforeVowel' [] isPrefix count = count
    countTheBeforeVowel' (x:xs) isPrefix count = countTheBeforeVowel' xs thePrefix r
        where
            thePrefix =
                case (notThe x) of
                    Nothing -> True
                    Just _ -> False
            r = case (notThe x) of
                    Just _ ->
                        if(isPrefix && beginsWithVowel x)
                            then (count + 1)
                            else count
                    Nothing -> count


