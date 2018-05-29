module Exercises where

  import Data.Monoid

  mySum :: (Foldable t, Num a) => t a -> a
  mySum xs = getSum $ foldMap Sum xs

  myProduct :: (Foldable t, Num a) => t a -> a
  myProduct xs = getProduct $ foldMap Product xs

  myElem :: (Foldable t, Eq a) => a -> t a -> Bool
  myElem a xa = getAny $ foldMap (\x -> Any (x == a)) xa
