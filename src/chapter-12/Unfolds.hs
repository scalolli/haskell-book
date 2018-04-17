{-# LANGUAGE ScopedTypeVariables #-}

module Unfolds where

    myIterate :: (a -> a) -> a -> [a]
    myIterate f a = a:(go f a)
            where go :: (a -> a) -> a -> [a]
                  go g b = (g b):(go g (g b))


    myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
    myUnfoldr f b =
            case f b of
                Just(x, y) -> x:(myUnfoldr f y)
                Nothing -> []

    myIterate' :: (a -> a) -> a -> [a]
    myIterate' f b = myUnfoldr (\x -> Just (x, (f x))) b