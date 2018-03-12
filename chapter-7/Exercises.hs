module Exercises where

tensDigit :: Integral a => a -> a

xPlaceDigit y x = r
    where (quot, remain) = x `divMod` y
          divMod' = divMod quot
          d = snd . divMod'
          r = d y

tensDigit = xPlaceDigit 10
hundresDigit = xPlaceDigit 100





