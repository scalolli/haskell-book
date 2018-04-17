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


myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []


myFilterWith :: (a -> Bool) -> a -> [a] -> [a]
myFilterWith f a xs = if(f a) then a:xs else xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (myFilterWith f) []


squish :: [[a]] -> [a]
squish = foldr (++) []


squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []


squishAgain :: [[a]] -> [a]
squishAgain = foldr ((++) . squishMap (\x -> [x])) []


myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl orderFMax x (x:xs)
          where orderFMax a b = if ((f a b) == GT) then a else b


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl orderFMin x (x:xs)
        where orderFMin a b = if ((f a b) == LT) then a else b