module Chapter16.Intermission where

  import Test.QuickCheck
  import Test.QuickCheck.Function

  a = fmap (+1) (read "[1]" :: [Int])

  b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

  c = fmap (*2) (\x -> x - 2) $ 1

  d = do
        fmap ((return '1' ++) . show) (\x -> [x, 1..3]) $ 0

--   e :: IO Integer
--   e = let ioi = readIO "1" :: IO Integer
--           changed = fmap ((read "123" :: Integer) ++ ) show ioi
--       in fmap (*3) changed

  functorIdentity :: (Functor f, (Eq (f a))) => f a -> Bool
  functorIdentity f =
        fmap id f == id f

  functorCompose :: (Functor f, (Eq (f c))) => (b -> c) -> (a -> b) -> f a -> Bool
  functorCompose f g x = ((fmap f . fmap g) x) == (fmap (f . g) x)

  f :: [Int] -> Bool
  f x = functorIdentity x

  fcompose :: [Int] -> Bool
  fcompose x = functorCompose (*2) (+1) x

  type IntInt = Fun Int Int

  fcompose' :: [Int] -> IntInt -> IntInt -> Bool
  fcompose' x (Fun _ f) (Fun _ g) = functorCompose f g x

-- Functor for Identity
  newtype Identity a = Identity a deriving (Eq, Show)

  instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

  instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
      a <- arbitrary
      return (Identity a)

  fIdentityForIdentity :: (Identity Int) -> Bool
  fIdentityForIdentity x = functorIdentity x

  fComposeForIdentity :: (Identity Int) -> IntInt -> IntInt -> Bool
  fComposeForIdentity x (Fun _ f) (Fun _ g) = functorCompose f g x

--   Functor for Pair a b

  data Pair a = Pair a a deriving (Eq, Show)

  instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

  instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
      a <- arbitrary
      return (Pair a a)

  fIdentityForPair :: (Pair Int) -> Bool
  fIdentityForPair x = functorIdentity x

  fComposeForPair :: (Pair Int) -> IntInt -> IntInt -> Bool
  fComposeForPair x (Fun _ f) (Fun _ g) = functorCompose f g x

--   Functor for Two

  data Two a b = Two a b deriving (Eq, Show)

  instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return (Two a b)

  functorIdentityForTwo :: (Two Int Int) -> Bool
  functorIdentityForTwo x = functorIdentity x

  fComposeForTwo :: (Two Int Int) -> IntInt -> IntInt -> Bool
  fComposeForTwo x (Fun _ f) (Fun _ g) = functorCompose f g x

-- Functor for Three

  data Three a b c = Three a b c deriving (Eq, Show)

  instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

  instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return (Three a b c)

  functorIdentityForThree :: (Three Int Int Int) -> Bool
  functorIdentityForThree x = functorIdentity x

  fComposeForThree :: (Three Int Int Int) -> IntInt -> IntInt -> Bool
  fComposeForThree x (Fun _ f) (Fun _ g) = functorCompose f g x


  chapter16Intermission :: IO ()
  chapter16Intermission = do
    quickCheck f
    quickCheck fcompose
    quickCheck fcompose'

    quickCheck fIdentityForIdentity
    quickCheck fComposeForIdentity

    quickCheck fIdentityForPair
    quickCheck fComposeForPair

    quickCheck functorIdentityForTwo
    quickCheck fComposeForTwo

    quickCheck functorIdentityForThree
    quickCheck fComposeForThree

