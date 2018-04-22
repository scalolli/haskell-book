module Spec where

  import Test.QuickCheck
  import Test.QuickCheck.Gen (oneof)
  import Test.Hspec
  import qualified Chapter14.QuickCheckExercisesSpec
  import qualified Chapter8.WordNumberSpec
  import qualified Chapter14.QuickCheckWithoutHspec
  import Chapter9.Cipher as C
  import Chapter11.CeaserCipher as Ceaser

  main :: IO ()
  main = do
    hspec Chapter14.QuickCheckExercisesSpec.spec
    hspec Chapter8.WordNumberSpec.spec
    Chapter14.QuickCheckWithoutHspec.main
    quickCheck property_cipher

  data Fool = Fulse | Frue deriving (Eq, Show)

  instance Arbitrary Fool where
    arbitrary = genFool

  genFool :: Gen Fool
  genFool =  frequency [(3, return Fulse), (1, return Frue)]

  genSafeWords :: Gen [Char]
  genSafeWords = listOf (elements (['a'..'z'] ++ ['A'..'Z']))

  property_cipher :: Property
  property_cipher = forAll genSafeWords (\x -> (C.decode (C.encode x)) == x)

--   property_CeaserCipher :: Property
--   property_CeaserCipher = forAll genSafeWords ()
