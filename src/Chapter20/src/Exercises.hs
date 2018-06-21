module Exercises where

  import Data.Monoid
  import Test.QuickCheck

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


  myExists :: a -> Maybe a -> Bool
  myExists = undefined


-- try custom gen for verifying my elem
--   customGen :: Gen (Integer, [Integer])
--   customGen = do
--     xs <- (arbitrary :: Gen [Integer])
--     x <- (arbitrary :: Gen Integer)
--     return (x `mod` ((length xs) - 1), xs)

  arbitraryForList :: Arbitrary a => Gen (a, [a])
  arbitraryForList = do 
      xs <- arbitrary      
      b <- choose (0, length xs - 1)
      return (xs !! b, xs)

  arbitraryForMaybeElem :: Arbitrary a => Gen (a, Maybe a)      
  arbitraryForMaybeElem = do 
      a <- arbitrary
      return (a, Just a)

  chapter20Intermissions:: IO ()
  chapter20Intermissions = do
    quickCheck $ property (forAll (arbitrary :: Gen [Integer]) validateMySum)
    quickCheck $ property (forAll (arbitrary :: Gen (Maybe Integer)) validateMySum)

    quickCheck $ property (forAll (arbitrary :: Gen [Integer]) validateMyProduct)
    quickCheck $ property (forAll (arbitrary :: Gen (Maybe Integer)) validateMyProduct)

    quickCheck $ property (forAll (arbitraryForList :: Gen (Int, [Int])) validateMyElem)
    quickCheck $ property (forAll (arbitraryForList :: Gen (Int, [Int])) validateMyElem)
    quickCheck $ property (forAll (arbitraryForMaybeElem :: Gen (Int, Maybe Int)) validateMyElemForMaybe)

    quickCheck $ property (forAll (arbitrary :: Gen [Integer]) (\xs -> (if (xs /= []) then Just $ minimum xs else Nothing) == myMinimum xs))
    quickCheck $ property (forAll (arbitrary :: Gen [Integer]) (\xs -> (if (xs /= []) then Just $ maximum xs else Nothing) == myMaximum xs))

  validateMySum :: (Foldable t, Eq a, Num a) => t a -> Bool
  validateMySum xs = sum xs == mySum xs

  validateMyProduct :: (Foldable t, Eq a, Num a) => t a -> Bool
  validateMyProduct xs = product xs == myProduct xs

  validateMyElem :: (Foldable t, Eq a, Num a) => (a, t a) -> Bool
  validateMyElem (a, xs) = elem a xs == myElem a xs

  validateMyElemForMaybe :: (Foldable t, Eq a) => (a, t a) -> Bool
  validateMyElemForMaybe (x, xs) = elem x xs == myElem x xs