module Chapter17.Exercises where

  import Data.Monoid
  import Test.QuickCheck
  import Test.QuickCheck.Function
  import Test.QuickCheck.Classes
  import Test.QuickCheck.Checkers

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

  chapter17TestExercises :: IO ()
  chapter17TestExercises = do
    quickBatch (monoid (undefined :: (Pair (String))))
    quickBatch (functor (undefined :: Pair (String, String, Int)))
    quickBatch (applicative (undefined :: Pair (String, String, Int)))

    quickBatch (monoid (undefined :: Two String String))
    quickBatch (functor (undefined :: Two String (String, String, String)))
    quickBatch (applicative (undefined :: Two String (String, String, String)))

