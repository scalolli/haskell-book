module Chapter14.QuickCheckExercisesSpec where

  import Test.QuickCheck
  import Test.Hspec
  import Data.List (sort)

  quickCheckUsingHspec :: IO ()
  quickCheckUsingHspec = hspec spec

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
        property property_plusAssociative

    describe "plus commutative" $ do
      it "should hold for Gen Int" $ do
        property property_plusCommutative

    describe "multiplication associative" $ do
      it "should hold for Gen Int" $ do
        property (forAll (arbitrary :: Gen (Int, Int, Int)) (\(x, y, z) -> property_multAssociative x y z))

    describe "multiplication commutative" $ do
      it "should hold for Gen Int" $ do
        property $ forAll (arbitrary :: Gen (Int, Int)) (\(x, y) -> property_multCommutative x y)


    describe "quotient remainder" $ do
      it "should hold for quotient and remainder" $ do
        property $ forAll tupleGenForDivision (\(x, y) -> property_qoutRemainder x y)

    describe "validate properties for functions" $ do
      it "should hold for division and mod" $ do
        property $ forAll tupleGenForDivision (\(x, y) -> property_divMod x y)

--       it "should check associative property for power function (^)" $ do
--         property $ forAll threeTupleGenForPower (\(x, y, z) -> property_powAssociative x y z)
--
--       it "should check commutative property for power function (^)" $ do
--         property $ forAll twoTupleGenForPower (\(x, y) -> property_powCommutative x y)

      it "should check if reversing a string twice returns the same string" $ do
        property property_reverse


  half :: Fractional a => a -> a
  half x = x / 2

  halfIdentity :: Fractional a => a -> a
  halfIdentity = (*2) . half

  property_plusAssociative :: Int -> Int -> Int -> Bool
  property_plusAssociative x y z = ((x + (y + z)) == ((x + y) + z))

  property_plusCommutative :: Int -> Int -> Bool
  property_plusCommutative x y = ((x + y) == (y + x))

  property_multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
  property_multAssociative x y z = ((x * (y * z)) == ((x * y) * z))

  property_multCommutative :: (Eq a, Num a) => a -> a -> Bool
  property_multCommutative x y = ((x * y) == (y * x))

  property_qoutRemainder :: Integral a => a -> a -> Bool
  property_qoutRemainder x y = (((quot x y)*y + (rem x y)) == x)

  property_divMod :: Integral a => a -> a -> Bool
  property_divMod x y = ((div x y)*y + (mod x y)) == x

  property_powAssociative :: Integral a => a -> a -> a -> Bool
  property_powAssociative x y z = ((x ^ y) ^ z) == (x ^ (y ^ z))

  property_powCommutative :: Integral a => a -> a -> Bool
  property_powCommutative x y = (x ^ y) == (y ^ x)

  tupleGenForDivision :: Gen (Int, Int)
  tupleGenForDivision = suchThat (arbitrary :: Gen (Int, Int)) (\(x, y) -> y /= 0)

  threeTupleGenForPower :: Gen (Int, Int, Int)
  threeTupleGenForPower = suchThat (arbitrary :: Gen (Int, Int, Int)) (\(x, y, z) -> x /= 0 && y /= 0 && z /= 0)

  twoTupleGenForPower :: Gen (Int, Int)
  twoTupleGenForPower = suchThat (arbitrary :: Gen (Int, Int)) (\(x, y) -> x /= 0 && y /= 0)

  property_reverse :: String -> Bool
  property_reverse xs = (reverse . reverse) xs == id xs

  listOrdered :: (Ord a) => [a] -> Bool
  listOrdered xs = snd $ foldr go (Nothing, True) xs
         where go _ status@(_, False) = status
               go y (Nothing, t) = (Just y, t)
               go y (Just x, t) = (Just y, x >= y)
