module Chapter17.Exercises where

  import Data.Monoid
  import Test.QuickCheck
  import Test.QuickCheck.Function
  import Test.QuickCheck.Classes
  import Test.QuickCheck.Checkers
  import Control.Applicative

  pureArr :: a -> [a]
  pureArr a = [a]

  apArr :: [(a -> b)] -> [a] -> [b]
  apArr = (<*>)

  pureIO :: a -> IO a
  pureIO = return

  appIO :: IO (a -> b) -> IO a -> IO b
  appIO = (<*>)

--   appIO (return (++ " World !")) getLine

  pureTuple :: Num b => a -> (Sum b, a)
  pureTuple a = (mempty, a)


  appTuple :: Num b => (Sum b, (a -> b)) -> (Sum b, a) -> (Sum b, b)
  appTuple (a, f) (b, x) = (a <> b, f x)

  pureFn :: b -> (a -> b)
  pureFn = pure

  appFn :: (a -> (e -> c)) -> (a -> e) -> (a -> c)
  appFn = (<*>)

  --   instances for Pair

  data Pair a = Pair a a deriving (Eq, Show)

  instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

  instance Eq a => EqProp (Pair a) where
    (=-=) = eq

  instance Monoid a => Monoid (Pair a) where
    mempty = Pair mempty mempty

    (Pair a b) `mappend` (Pair c d) = Pair (mappend a c) (mappend b d)

  instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

  instance Applicative Pair where
    pure a = Pair a a

    (Pair f g) <*> (Pair a b) = Pair (f a) (g b)

  -- instances for Two

  data Two a b = Two a b deriving (Eq, Show)

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

  instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

  instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty

    (Two a b) `mappend` (Two c d) = Two (a `mappend` c) (b `mappend` d)

  instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

  instance Monoid a => Applicative (Two a) where
    pure x = Two mempty x

    (Two a f) <*> (Two c d) = Two (a <> c) (f d)

--     instances for Three

  data Three a b c = Three a b c deriving (Eq, Show)

  instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

  instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

  instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
    mempty = Three mempty mempty mempty

    (Three a b c) `mappend` (Three d e f) = Three (a `mappend` d) (b `mappend` e) (c `mappend` f)

  instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

  instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure x = Three mempty mempty x

    (Three a b f) <*> (Three c d e) = Three (a <> c) (b <> d) (f e)

-- instances for Three'

  data Three' a b = Three' a b b deriving (Eq, Show)

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

  instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

  instance (Monoid a, Monoid b) => Monoid (Three' a b) where
    mempty = Three' mempty mempty mempty

    (Three' a b c) `mappend` (Three' d e f) = Three' (a `mappend` d) (b `mappend` e) (c `mappend` f)

  instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

  instance (Monoid a) => Applicative (Three' a) where
    pure x = Three' mempty x x

    (Three' a f g) <*> (Three' c d e) = Three' (a <> c) (f d) (g e)

--     instances for Four

  data Four a b c d = Four a b c d deriving (Eq, Show)

  instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

  instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq

  instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
    mempty = Four mempty mempty mempty mempty

    (Four a b c d) `mappend` (Four e f g h) = Four (a `mappend` e) (b `mappend` f) (c `mappend` g) (d `mappend` h)

  instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

  instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure x = Four mempty mempty mempty x

    (Four a b c f) <*> (Four d e x y) = Four (a <> d) (b <> e) (c <> x) (f y)

-- instances for Four'

  data Four' a b = Four' a a a b deriving (Eq, Show)

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

  instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq

  instance (Monoid a, Monoid b) => Monoid (Four' a b) where
    mempty = Four' mempty mempty mempty mempty

    (Four' a b c d) `mappend` (Four' e f g h) = Four' (a `mappend` e) (b `mappend` f) (c `mappend` g) (d `mappend` h)

  instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

  instance (Monoid a) => Applicative (Four' a) where
    pure x = Four' mempty mempty mempty x

    (Four' a b c fn) <*> (Four' d e f g) = Four' (a <> d) (b <> e) (c <> f) (fn g)


  stops :: String
  stops = "pbtdkg"

  vowels :: String
  vowels = "aeiou"

  combos :: [a] -> [b] -> [c] -> [(a, b, c)]
  combos xs ys zs = liftA3 (,,) xs ys zs

  chapter17TestExercises :: IO ()
  chapter17TestExercises = do
    quickBatch (monoid (undefined :: (Pair (String))))
    quickBatch (functor (undefined :: Pair (String, String, Int)))
    quickBatch (applicative (undefined :: Pair (String, String, Int)))

    quickBatch (monoid (undefined :: Two String String))
    quickBatch (functor (undefined :: Two String (String, String, String)))
    quickBatch (applicative (undefined :: Two String (String, String, String)))

    quickBatch (monoid (undefined :: Three String String String))
    quickBatch (functor (undefined :: Three String String (String, String, String)))
    quickBatch (applicative (undefined :: Three String String (String, String, String)))

--  type Three' String (String, String, String) means Three' String implies the functor instance for (Three
    quickBatch (monoid (undefined :: Three' String String))
    quickBatch (functor (undefined :: Three' String (String, String, String)))
    quickBatch (applicative (undefined :: Three' String (String, String, String)))

    quickBatch (monoid (undefined :: Four String String String String))
    quickBatch (functor (undefined :: Four String String String (String, String, String)))
    quickBatch (applicative (undefined :: Four String String String (String, String, String)))

    quickBatch (monoid (undefined :: Four' String String))
    quickBatch (functor (undefined :: Four' String (String, String, String)))
    quickBatch (applicative (undefined :: Four' String (String, String, String)))

    putStrLn $ "Stop Vowel Stop combinations:- " ++ (show $ combos stops vowels stops)

