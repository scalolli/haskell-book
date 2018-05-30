module Exercises where

  import Data.Monoid

  mySum :: (Foldable t, Num a) => t a -> a
  mySum xs = getSum $ foldMap Sum xs

  myProduct :: (Foldable t, Num a) => t a -> a
  myProduct xs = getProduct $ foldMap Product xs

  myElem :: (Foldable t, Eq a) => a -> t a -> Bool
  myElem a xa = getAny $ foldMap (\x -> Any (x == a)) xa

  myMinimum :: (Foldable t, Ord a) => t a -> Maybe a
  myMinimum = foldr (compareMaybe (<)) Nothing

  myMaximum :: (Foldable t, Ord a) => t a -> Maybe a
  myMaximum = foldr (compareMaybe (>)) Nothing


  compareMaybe :: Ord a => (a -> a -> Bool) -> a -> Maybe a -> Maybe a
  compareMaybe fn a b =
    case fmap (fn a) b of
      Just True -> Just a
      Just False -> b
      Nothing    -> Just a
