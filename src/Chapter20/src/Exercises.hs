module Exercises where

  import Data.Monoid
  import Data.Foldable
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


  isNull :: Foldable t => t a -> Bool
  isNull xs = foldr (\_ _ -> False) True xs

  myLength :: Foldable t => t a -> Int
  myLength xs = getSum $ foldMap (\x -> Sum 1) xs

  myToList :: Foldable t => t a -> [a]
  myToList = foldr (\a b -> a:b) []

  myFold :: (Foldable t, Monoid m) => t m -> m
  myFold = foldMap id

  myFoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
  myFoldMap f xs = foldr (\a b -> mappend (f a) b) mempty xs

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

    quickCheck $ property (forAll (arbitrary :: Gen [Integer]) (\xs -> null xs == isNull xs))
    quickCheck $ property (forAll (arbitrary :: Gen (Maybe Integer)) (\xs -> null xs == isNull xs))

    quickCheck $ property (forAll (arbitrary :: Gen [Integer]) (\xs -> length xs == myLength xs))
    quickCheck $ property (forAll (arbitrary :: Gen (Maybe Integer)) (\xs -> length xs == myLength xs))

    quickCheck $ property (forAll (arbitrary :: Gen (Maybe Integer)) (\xs -> toList xs == myToList xs))
    quickCheck $ property (forAll (arbitrary :: Gen [Integer]) (\xs -> toList xs == myToList xs))
    
    quickCheck $ property (forAll ((fmap . fmap) Sum $ arbitrary :: Gen [Sum Int]) (\xs -> fold xs == myFold xs))
    quickCheck $ property (forAll ((fmap . fmap) Product $ arbitrary :: Gen [Product Int]) (\xs -> fold xs == myFold xs))
    
    quickCheck $ property (forAll (arbitrary :: Gen [Int]) (\xs -> foldMap Sum xs == myFoldMap Sum xs))

  validateMySum :: (Foldable t, Eq a, Num a) => t a -> Bool
  validateMySum xs = sum xs == mySum xs

  validateMyProduct :: (Foldable t, Eq a, Num a) => t a -> Bool
  validateMyProduct xs = product xs == myProduct xs

  validateMyElem :: (Foldable t, Eq a, Num a) => (a, t a) -> Bool
  validateMyElem (a, xs) = elem a xs == myElem a xs

  validateMyElemForMaybe :: (Foldable t, Eq a) => (a, t a) -> Bool
  validateMyElemForMaybe (x, xs) = elem x xs == myElem x xs