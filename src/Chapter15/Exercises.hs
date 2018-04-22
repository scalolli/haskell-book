module Chapter15.Exercises where

  import Test.QuickCheck
  import Data.Semigroup

-- Semigroup for Trivial

  data Trivial = Trivial deriving (Eq, Show)

  instance Semigroup Trivial where
    (<>) _ _ = Trivial

  instance Arbitrary Trivial where
    arbitrary = return Trivial

  semiGroupAssoc :: (Semigroup a, Eq a) => a -> a -> a -> Bool
  semiGroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

  type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool


-- Semigroup for Identity

  newtype Identity a = Identity a
      deriving (Eq, Show)

  instance Semigroup a => Semigroup (Identity a) where
      (Identity x) <> (Identity y) = Identity (x <> y)

  instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
      x <- arbitrary
      return (Identity x)

  type IdentityAssoc = (Identity String) -> (Identity String) -> (Identity String) -> Bool

-- Semigroup for Two

  data Two a b = Two a b deriving (Eq, Show)

  instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (Two x y)

  instance Semigroup Int where
    a <> b = a + b

  type TwoAssoc = (Two String Int) -> (Two String Int) -> (Two String Int) -> Bool

-- Semigroup for three

  data Three a b c = Three a b c deriving (Eq, Show)

  instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three i j k) <> (Three l m n) = Three (i <> l) (j <> m) (k <> n)

  instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      return (Three x y z)

  type ThreeAssoc = (Three String Int Int) -> (Three String Int Int) -> (Three String Int Int) -> Bool

-- Semigroup for four

  data Four a b c d = Four a b c d deriving (Eq, Show)

  instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    (Four h i j k) <> (Four l m n o) = Four (h <> l) (i <> m) (j <> n) (k <> o)

  instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
      w <- arbitrary
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      return (Four w x y z)

  type FourAssoc = (Four Int String Int Int) -> (Four Int String Int Int) -> (Four Int String Int Int) -> Bool


-- Semigroup for BoolConj

  newtype BoolConj = BoolConj Bool deriving (Eq, Show)

  instance Semigroup BoolConj where
    (BoolConj True) <> (BoolConj True) = BoolConj True
    (BoolConj False) <> _ = BoolConj False
    _ <> (BoolConj False) = BoolConj False

  instance Arbitrary BoolConj where
    arbitrary = elements [BoolConj True, BoolConj False]

  type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool


-- Semigroup for BoolDisj

  newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

  instance Semigroup BoolDisj where
    (BoolDisj True) <> (BoolDisj True) = BoolDisj True
    (BoolDisj True) <> _ = BoolDisj True
    _ <> (BoolDisj True) = BoolDisj True
    _ <> _ = BoolDisj False


  instance Arbitrary BoolDisj where
    arbitrary = elements [BoolDisj True, BoolDisj False]

  type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool


--  Semigroup for Or

  data Or a b = Fst a | Snd b deriving (Eq, Show)

  instance Semigroup (Or a b) where
    (Snd a) <> (Snd b) = Snd a
    (Fst a) <> (Fst b) = Fst b
    (Snd a) <> _ = Snd a
    _ <> (Snd b) = Snd b

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      elements [Fst x, Snd y]

  type OrAssoc = (Or String Int) -> (Or String Int) -> (Or String Int) -> Bool

-- Semigroup for Combine

  newtype Combine a b = Combine {unCombine :: a -> b}

  instance Show (Combine a b) where
    show (Combine _) = "nothing useful"

  instance Semigroup b => Semigroup (Combine a b) where
    x <> y = Combine {unCombine = (\c -> ((unCombine x) c) <> ((unCombine y) c))}

  e :: Combine Int (Sum Int)
  e = Combine $ (\n -> Sum (n + 1))

  f :: Combine Int (Sum Int)
  f = Combine $ (\n -> Sum (n + 1))

  g :: Combine Int (Sum Int)
  g = Combine $ \n -> Sum (n - 1)

  h = (unCombine (e <> f <> g)) 1

  combineArbitrary = do
          w <- (arbitrary :: Gen Int)
          x <- (arbitrary :: Gen (Int -> Int))
          y <- (arbitrary :: Gen (Int -> Int))
          z <- (arbitrary :: Gen (Int -> Int))
          return (w, Combine x, Combine y , Combine z)

  type S = Combine Int Int

  combineAssoc :: (Int, S, S, S) -> Bool
  combineAssoc (w, x, y, z) = ((unCombine (x <> (y <> z))) w) == ((unCombine ((x <> y) <> z)) w)

  chapter15Exercises :: IO ()
  chapter15Exercises = do
    quickCheck (semiGroupAssoc :: TrivialAssoc)
    quickCheck (semiGroupAssoc :: IdentityAssoc)
    quickCheck (semiGroupAssoc :: TwoAssoc)
    quickCheck (semiGroupAssoc :: ThreeAssoc)
    quickCheck (semiGroupAssoc :: FourAssoc)
    quickCheck (semiGroupAssoc :: BoolConjAssoc)
    quickCheck (semiGroupAssoc :: BoolDisjAssoc)
    quickCheck (semiGroupAssoc :: OrAssoc)
    quickCheck $ forAll combineArbitrary combineAssoc


