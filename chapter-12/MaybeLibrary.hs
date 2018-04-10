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



