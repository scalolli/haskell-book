
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


-- Generators

acro :: String -> String
acro xs = [x | x <- xs, x `elem` ['A'..'Z']]


mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]
myTuples = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

myLength :: [a] -> Integer
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

itIsMystery :: [Char] -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs

multiplesOfThree :: [Integer] -> [Integer]
multiplesOfThree xs = filter (\x -> (x `mod` 3) == 0) xs

lengthOfMultiples =  length . multiplesOfThree

filterArticles xs = filter (\x -> (x /= "the") || (x /= "a") || (x /= "an")) (words xs)


zip' :: [a] -> [b] -> [(a,b)]
zip' = zipWith' (\x -> \y -> (x, y))

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = (f x y) : (zipWith' f xs ys)
