module EitherLibrary where

    lefts' :: [Either a b] -> [a]
    lefts' = foldr go []
        where
            go (Left a) acc = a:acc
            go _ acc = acc


    rights' :: [Either a b] -> [b]
    rights' = foldr go []
        where
            go (Right a) acc = a:acc
            go _ acc = acc

    partitionEithers' :: [Either a b] -> ([a], [b])
    partitionEithers' = foldr go ([], [])
        where
            go (Left a) (ls, rs) = (a:ls, rs)
            go (Right b) (ls, rs) = (ls, b:rs)


