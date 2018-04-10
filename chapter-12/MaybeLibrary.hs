module MaybeLibrary where

    isJust :: Maybe a -> Bool
    isJust Nothing = False
    isJust _ = True

    isNothing :: Maybe a -> Bool
    isNothing Nothing = True
    isNothing _ = False

    mayybee :: b -> (a -> b) -> Maybe a -> b
    mayybee b f Nothing = b
    mayybee b f (Just x) = f x

    mayybee' :: b -> (a -> b -> b) -> Maybe a -> b
    mayybee' b f Nothing = b
    mayybee' b f (Just x) = f x b

    fromMaybe :: a -> Maybe a -> a
    fromMaybe a maybe = mayybee a id maybe


    listToMaybe :: [a] -> Maybe a
    listToMaybe [] = Nothing
    listToMaybe (x:xs) = Just x

    maybeToList :: Maybe a -> [a]
    maybeToList Nothing = []
    maybeToList (Just a) = [a]

    catMaybes :: [Maybe a] -> [a]
    catMaybes [] = []
    catMaybes ((Just a):xs) = a:(catMaybes xs)
    catMaybes ((Nothing):xs) = catMaybes xs

    flipMaybe :: [Maybe a] -> Maybe [a]
    flipMaybe xs = go xs []
        where
            go [] acc = Just acc
            go ((Just a):ys) acc = go ys (a:acc)
            go ((Nothing):ys) _ = Nothing


