
myHead :: [a] -> Maybe a
myHead ([]) = Nothing
myHead (head : xs) = Just head

myTail :: [a] -> Maybe [a]
myTail ([]) = Nothing
myTail (_ : xs) = Just xs


enumFromTo' :: (Enum a, Ord a) => a -> a -> [a]
enumFromTo' a b
    | a <= b = a : (enumFromTo' (succ a) b)
    | otherwise = []


myWords' :: String -> Char -> [String]
myWords' [] _ = []
myWords' sentence sep = word : (myWords' nextPhrase sep)
            where
                nextPhrase = dropWhile (== sep) (dropWhile (/= sep) sentence)
                word = (takeWhile (/= sep) sentence)

myWords :: String -> [String]
myWords [] = []
myWords sentence = myWords' sentence ' '

myLines :: String -> [String]
myLines a = myWords' a '\n'

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

shouldEqual =
     [ "Tyger Tyger, burning bright"
     , "In the forests of the night"
     , "What immortal hand or eye"
     , "Could frame thy fearful symmetry?"
     ]

main :: IO ()
main = print $ "Are they equal " ++ show (myLines sentences == shouldEqual)
