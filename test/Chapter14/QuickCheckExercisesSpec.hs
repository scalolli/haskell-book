module Chapter14.QuickCheckExercisesSpec where

  import Test.QuickCheck
  import Test.Hspec
  import Data.List (sort)

  spec :: Spec
  spec = do
    describe "halfIdentity" $ do
      it "should verify halfIdentity = (*2) . half for Gen Double" $ do
        property
           (forAll (arbitrary :: (Gen Double))
              (\x -> (x == halfIdentity x)))

      it "should verify halfIdentity = (*2) . half for Gen Float" $ do
        property
           (forAll (arbitrary :: (Gen Float))
              (\x -> (x == halfIdentity x)))

    describe "sort" $ do
      it "should verify listOrdered for [Int]" $ do
        property
          (forAll (arbitrary :: Gen [Int])
          (\xs -> listOrdered (sort xs)))

      it "should verify listOrdered for [Char]" $ do
        property
          (forAll (arbitrary :: Gen [Char])
          (\xs -> listOrdered (sort xs)))

    describe "plusAssociative" $ do
      it "should hold for Gen Int" $ do
        property
          (forAll (arbitrary :: Gen (Int, Int, Int))
           (\(x, y, z) -> property_plusAssociative x y z))


  half :: Fractional a => a -> a
  half x = x / 2

  halfIdentity :: Fractional a => a -> a
  halfIdentity = (*2) . half

  property_plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
  property_plusAssociative x y z = ((x + (y + z)) == ((x + y) + z))


  listOrdered :: (Ord a) => [a] -> Bool
  listOrdered xs = snd $ foldr go (Nothing, True) xs
         where go _ status@(_, False) = status
               go y (Nothing, t) = (Just y, t)
               go y (Just x, t) = (Just y, x >= y)



