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
    countTheBeforeVowel' [] _ count = count
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


    countVowels :: String -> Integer
    countVowels xs = go xs 0
        where
            go [] acc = acc
            go (x:xs) acc = go xs count
                where count =
                        if(x `elem` "aeiou")
                            then (acc + 1)
                            else acc


    newtype Word' = Word' String deriving (Eq, Show)
    vowels = "aeiou"

    mkWord :: String -> Maybe Word'
    mkWord w = if(vowelCount > consCount) then Nothing else Just (Word' w)
        where
            (vowelCount, consCount) = foldr (\x (vc, cc) -> if(x `elem` vowels) then (vc+1, cc) else (vc, cc+1)) (0,0) w



