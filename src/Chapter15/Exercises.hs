module Chapter15.Exercises where

  import Test.QuickCheck
  import Data.Semigroup

-- Semigroup and Monoid for Trivial

  data Trivial = Trivial deriving (Eq, Show)

  instance Semigroup Trivial where
    (<>) _ _ = Trivial

  instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

  instance Arbitrary Trivial where
    arbitrary = return Trivial

  semiGroupAssoc :: (Semigroup a, Eq a) => a -> a -> a -> Bool
  semiGroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

  type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

  mli :: (Monoid a, Eq a, Semigroup a) => a -> Bool
  mli x = (mempty <> x) == x

  mri :: (Monoid a, Eq a, Semigroup a) => a -> Bool
  mri x = x == (x <> mempty)


-- Semigroup and Monoid for Identity

  newtype Identity a = Identity a
      deriving (Eq, Show)

  instance Semigroup a => Semigroup (Identity a) where
      (Identity x) <> (Identity y) = Identity (x <> y)

  instance (Semigroup a, Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)


  instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
      x <- arbitrary
      return (Identity x)

  type IdentityAssoc = (Identity String) -> (Identity String) -> (Identity String) -> Bool

-- Semigroup and Monoid for Two

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

  instance (Semigroup a, Monoid a, Semigroup b, Monoid b) => Monoid (Two a b) where
    mempty = Two (mempty) (mempty)
    mappend = (<>)

  type TwoAssoc = (Two String Int) -> (Two String Int) -> (Two String Int) -> Bool

-- Semigroup and Monoid for three

  data Three a b c = Three a b c deriving (Eq, Show)

  instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three i j k) <> (Three l m n) = Three (i <> l) (j <> m) (k <> n)

  instance (Semigroup a, Monoid a, Semigroup b, Monoid b, Semigroup c, Monoid c) => Monoid (Three a b c) where
    mempty = Three (mempty) (mempty) (mempty)
    mappend = (<>)

  instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      z <- arbitrary
      return (Three x y z)

  type ThreeAssoc = (Three String Int Int) -> (Three String Int Int) -> (Three String Int Int) -> Bool

-- Semigroup and Monoid for four

  data Four a b c d = Four a b c d deriving (Eq, Show)

  instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    (Four h i j k) <> (Four l m n o) = Four (h <> l) (i <> m) (j <> n) (k <> o)

  instance (Semigroup a, Monoid a, Semigroup b, Monoid b, Semigroup c, Monoid c, Semigroup d, Monoid d) => Monoid (Four a b c d) where
    mempty = Four (mempty) (mempty) (mempty) (mempty)
    mappend = (<>)

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

  instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

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

  instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)


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

  instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
    mempty = Combine {unCombine = (\a -> mempty)}
    mappend = (<>)

  e :: Combine Int (Sum Int)
  e = Combine $ (\n -> Sum (n + 1))

  f :: Combine Int (Sum Int)
  f = Combine $ (\n -> Sum (n + 1))

  g :: Combine Int (Sum Int)
  g = Combine $ \n -> Sum (n - 1)

  h = (unCombine (e <> f <> g)) 1

  combineArbitrary = do
          w <- (arbitrary :: Gen Int)
          x <- (arbitrary :: Gen (Int -> String))
          y <- (arbitrary :: Gen (Int -> String))
          z <- (arbitrary :: Gen (Int -> String))
          return (w, Combine x, Combine y , Combine z)

  type S = Combine Int String

  combineAssoc :: (Int, S, S, S) -> Bool
  combineAssoc (w, x, y, z) = ((unCombine (x <> (y <> z))) w) == ((unCombine ((x <> y) <> z)) w)

  combineFnArbitrary :: Gen (Int, (Combine Int String))
  combineFnArbitrary = do
    a <- arbitrary :: Gen Int
    f <- (arbitrary :: Gen (Int -> String))
    return (a, Combine f)

  combineMli :: (Int, Combine Int String) -> Bool
  combineMli (a, x) = ((unCombine (mempty <> x)) a) == ((unCombine x) a)

  combineMri :: (Int, Combine Int String) -> Bool
  combineMri (a, x) = ((unCombine (x <> mempty)) a) == ((unCombine x) a)


-- SemiGroup for Comp

  newtype Comp a = Comp {unComp :: a -> a}

  instance Show (Comp a) where
    show (Comp _) = "Nothing useful"

  instance Semigroup a => Semigroup (Comp a) where
    (Comp f) <> (Comp g) = Comp {unComp = (f . g)}

  instance Semigroup a => Monoid (Comp a) where
    mempty = Comp id
    mappend = (<>)


  compArbitrary = do
          w <- (arbitrary :: Gen Int)
          x <- (arbitrary :: Gen (Int -> Int))
          y <- (arbitrary :: Gen (Int -> Int))
          z <- (arbitrary :: Gen (Int -> Int))
          return (w, Comp x, Comp y , Comp z)

  type CompFn = (Comp Int)

  compAssoc :: (Int, CompFn, CompFn, CompFn) -> Bool
  compAssoc (w, x, y, z) = ((unComp (x <> (y <> z))) w) == ((unComp ((x <> y) <> z)) w)

--  Semigroup for Validation

  data Validation a b =
    Failure' a | Success' b
    deriving (Eq, Show)

  instance Semigroup a => Semigroup (Validation a b) where
    (Failure' x) <> (Failure' y) = Failure' (x <> y)
    (Success' x) <> (Success' y) = Success' x
    (Success' x) <> (Failure' y) = Success' x
    (Failure' x) <> (Success' y) = Success' y

  failure :: String -> Validation String Int
  failure = Failure'

  success :: Int -> Validation String Int
  success = Success'

--   Monoid for Mem

  newtype Mem s a =
    Mem {
      runMem :: s -> (a, s)
    }

  instance Show (Mem s a) where
    show _ = "Place holder"

  resultantFn :: Semigroup a => (b -> (a, b)) -> (b -> (a, b)) -> b -> (a, b)
  resultantFn f g b = (a1 <> a2, b2)
        where
            (a1, b1) = g b
            (a2, b2) = f b1

  instance Semigroup a => Semigroup (Mem s a) where
    Mem f <> Mem g = Mem {runMem = (resultantFn f g)}

  instance (Monoid a, Semigroup a) => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)
    mappend = (<>)

  arbitraryMemForIdentity :: Gen (Int, Mem Int String)
  arbitraryMemForIdentity = do
    x <- (arbitrary :: Gen Int)
    f <- (arbitrary :: Gen (Int -> (String, Int)))
    return (x, Mem f)

  arbitraryMemForAssoc :: Gen (Int, Mem Int String, Mem Int String)
  arbitraryMemForAssoc = do
    x <- (arbitrary :: Gen Int)
    f <- (arbitrary :: Gen (Int -> (String, Int)))
    g <- (arbitrary :: Gen (Int -> (String, Int)))
    return (x, Mem f, Mem g)

  memRi :: (Int, (Mem Int String)) -> Bool
  memRi (b, m@(Mem f)) = (runMem (m <> mempty) b) == (runMem m b)

  memLi :: (Int, (Mem Int String)) -> Bool
  memLi (b, m@(Mem f)) = (runMem (mempty <> m) b) == (runMem m b)

  f' = Mem $ \s -> ("hi", s + 1)

  chapter15Exercises :: IO ()
  chapter15Exercises = do
    quickCheck (semiGroupAssoc :: TrivialAssoc)
    quickCheck (mli :: Trivial -> Bool)
    quickCheck (mri :: Trivial -> Bool)

    quickCheck (semiGroupAssoc :: IdentityAssoc)
    quickCheck (mli :: ((Identity String) -> Bool))
    quickCheck (mri :: ((Identity String) -> Bool))

    quickCheck (semiGroupAssoc :: TwoAssoc)
    quickCheck (mli :: ((Two String String) -> Bool))
    quickCheck (mri :: ((Two String String) -> Bool))

    quickCheck (semiGroupAssoc :: ThreeAssoc)
    quickCheck (mli :: ((Three String String String) -> Bool))
    quickCheck (mri :: ((Three String String String) -> Bool))

    quickCheck (semiGroupAssoc :: FourAssoc)
    quickCheck (mli :: ((Four String String String String) -> Bool))
    quickCheck (mri :: ((Four String String String String) -> Bool))

    quickCheck (semiGroupAssoc :: BoolConjAssoc)
    quickCheck (mli :: ((BoolConj) -> Bool))
    quickCheck (mri :: ((BoolConj) -> Bool))

    quickCheck (semiGroupAssoc :: BoolDisjAssoc)
    quickCheck (mli :: ((BoolDisj) -> Bool))
    quickCheck (mri :: ((BoolDisj) -> Bool))


    quickCheck (semiGroupAssoc :: OrAssoc)
    quickCheck $ forAll combineArbitrary combineAssoc
    quickCheck $ forAll combineFnArbitrary combineMli
    quickCheck $ forAll combineFnArbitrary combineMri

    quickCheck $ forAll compArbitrary compAssoc
    print $ success 1 <> failure "blah"
    print $ failure "woot" <> failure "blah"
    print $ success 1 <> success 2
    print $ failure "woot" <> success 2

    let rmzero = runMem mempty 0
        rmleft = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0
    print $ rmleft
    print $ rmright
    print $ (rmzero :: (String, Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0

    quickCheck $ forAll arbitraryMemForIdentity memLi
    quickCheck $ forAll arbitraryMemForIdentity memRi


