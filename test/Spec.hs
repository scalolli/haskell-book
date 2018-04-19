module Spec where

  import Test.QuickCheck
  import Test.QuickCheck.Gen (oneof)
  import Test.Hspec
  import qualified Chapter14.QuickCheckExercisesSpec
  import qualified Chapter8.WordNumberSpec
  import qualified Chapter14.QuickCheckWithoutHspec


  main :: IO ()
  main = do
    hspec Chapter14.QuickCheckExercisesSpec.spec
    hspec Chapter8.WordNumberSpec.spec
    Chapter14.QuickCheckWithoutHspec.main


  data Fool = Fulse | Frue deriving (Eq, Show)

  instance Arbitrary Fool where
    arbitrary = genFool

  genFool :: Gen Fool
  genFool =  frequency [(3, return Fulse), (1, return Frue)]
