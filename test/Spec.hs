module Spec where

  import Test.QuickCheck
  import Test.Hspec
  import qualified Chapter14.QuickCheckExercisesSpec
  import qualified Chapter8.WordNumberSpec
  import qualified Chapter14.QuickCheckWithoutHspec


  main :: IO ()
  main = do
    hspec Chapter14.QuickCheckExercisesSpec.spec
    hspec Chapter8.WordNumberSpec.spec
    Chapter14.QuickCheckWithoutHspec.main

  property_foldr :: [Int] -> Bool
  property_foldr xs = (foldr (:) [] xs) == (xs ++ [])