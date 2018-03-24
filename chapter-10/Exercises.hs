module Exercises where


stops = "pbtdkg"
vowels = "aeiou"

stopVowelStop :: [(Char, Char, Char)]
stopVowelStop = [(x,y,z) | x <- stops, y <- vowels, z <- stops, x /= 'p']

nouns = ["basu", "home", "car", "anusha"]
verbs = ["run", "walk", "crawl", "write", "sleep", "work", "eat", "do isshi"]

nounVerbNoun :: [(String, String, String)]
nounVerbNoun = [(n, v, n') | n <- nouns, v <- verbs, n' <- nouns, n == "anusha"]

seekritFunc :: (Fractional a) => String -> a
seekritFunc x = (fromIntegral $ sum (map length (words x))) / (fromIntegral $ length (words x))


myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myEven :: Integer -> Bool -> Bool
myEven = (||) . even

evenOrF :: Integer -> Bool -> Bool
evenOrF = (||) . even

myAny :: [Integer] -> Bool
myAny = foldr evenOrF False


myElemF x = (||) . ( == x)
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (myElemF x) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = any (==e)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

