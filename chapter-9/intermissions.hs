
myHead :: [a] -> Maybe a
myHead ([]) = Nothing
myHead (head : xs) = Just head

myTail :: [a] -> Maybe [a]
myTail ([]) = Nothing
myTail (_ : xs) = Just xs


enumFromTo' :: (Enum a, Ord a) => a -> a -> [a]
enumFromTo' a b = go a []
    where go a' acc
            | a' >= b = acc ++ [b]
            | otherwise = go next (acc ++ [a'])
                where
                    next = succ a'
