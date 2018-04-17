module Exercises where

tensDigit :: Integral a => a -> a
hundresDigit :: Integral a => a -> a

xPlaceDigit :: (Integral a) => a -> a -> a
xPlaceDigit y x = r
    where (quot', _) = x `divMod` y
          divMod' = divMod quot'
          d = snd . divMod'
          r = d y

tensDigit = xPlaceDigit 10
hundresDigit = xPlaceDigit 100


foldBool :: a -> a -> Bool -> a
-- foldBool x _ False = x
-- foldBool _ y True = y

-- foldBool x y z =
--       case z of
--          False -> x
--          True -> y

foldBool x y z
    | z == False = x
    | otherwise = y


g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)



