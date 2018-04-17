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


    either' :: (a -> c) -> (b -> c) -> Either a b -> c
    either' fa _ (Left a) = fa a
    either' _ fb (Right b) = fb b

    eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
    eitherMaybe' _ (Left _) = Nothing
    eitherMaybe' f e@(Right b) = Just (either' (\x -> f b) f e)