module Chapter8.WordNumberSpec where

  import Chapter8.WordNumber
  import Test.Hspec
  import Test.QuickCheck

  half x = x / 2
  halfIdentity = (*2) . half

  spec :: Spec
  spec = do
    describe "Prelude.read" $ do
      it "returns zero for 0" $ do
        digitToWord 0 `shouldBe` "zero"

      it "returns one for 1" $ do
        digitToWord 1 `shouldBe` "one"

    describe "digits" $ do
      it "returns [1] for 1" $ do
        digits 1 `shouldBe` [1]

      it "returns [1, 0, 0]" $ do
        digits 100 `shouldBe` [1, 0, 0]

    describe "wordNumber" $ do
      it "one-zero-zero given 100" $ do
        wordNumber 100 `shouldBe` "one-zero-zero"

      it "nine-zero-zero-one for 9001" $ do
        wordNumber 9001 `shouldBe` "nine-zero-zero-one"

    describe "quick check example" $ do
      it "x/2 should be always half" $ do
        property $ \x -> x + 1 > (x :: Int)

