module Factorial where

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times - 1) n)

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' 0 n = n
incTimes' times n = applyTimes times (+1) n


f :: Bool -> Maybe Int
f False = Just 0
f _     = Nothing

fibonacci:: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)



