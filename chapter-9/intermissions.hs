
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
