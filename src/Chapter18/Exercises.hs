module Chapter18.Exercises where

  import Test.QuickCheck
  import Data.Semigroup
  import Test.QuickCheck.Function
  import Test.QuickCheck.Classes
  import Test.QuickCheck.Checkers
  import Control.Monad

  data Nope a = NopeDotJpg deriving (Eq, Show)

  instance Monoid (Nope a) where
    mempty = NopeDotJpg

    NopeDotJpg `mappend` NopeDotJpg = NopeDotJpg

  instance Functor Nope where
    fmap _ NopeDotJpg = NopeDotJpg

  instance Applicative Nope where
    pure = return NopeDotJpg

    NopeDotJpg <*> NopeDotJpg = NopeDotJpg

  instance Monad Nope where
    return = pure

    NopeDotJpg >>= _ = NopeDotJpg

  instance Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

  instance EqProp (Nope a) where (=-=) = eq

--   instance for Identity

  newtype MyIdentityAgain a = MyIdentityAgain a  deriving (Eq, Ord, Show)

  instance Monoid a => Monoid (MyIdentityAgain a) where
    mempty = MyIdentityAgain mempty

    (MyIdentityAgain a) `mappend` (MyIdentityAgain b) = MyIdentityAgain (a `mappend` b)

  instance Functor MyIdentityAgain where
    fmap f (MyIdentityAgain a) = MyIdentityAgain (f a)

  instance Applicative MyIdentityAgain where
    pure = MyIdentityAgain

    MyIdentityAgain f <*> MyIdentityAgain a = MyIdentityAgain (f a)

  instance Monad MyIdentityAgain where
    return = MyIdentityAgain

    (MyIdentityAgain a) >>= f = f a

  instance Arbitrary a => Arbitrary (MyIdentityAgain a) where
    arbitrary = MyIdentityAgain <$> arbitrary

  instance Eq a => EqProp (MyIdentityAgain a) where (=-=) = eq

-- List Monad

  data ListChp18 a = ListNil | ListChp18Cons a (ListChp18 a) deriving (Eq, Show)

  instance Monoid (ListChp18 a) where
    mempty = ListNil

    ListNil `mappend` x  = x
    x `mappend` ListNil = x
    (ListChp18Cons x xs) `mappend` other@(ListChp18Cons y ys) = ListChp18Cons  x $ xs `mappend` other

  instance Functor ListChp18 where
    fmap f ListNil = ListNil
    fmap f (ListChp18Cons x xs) = ListChp18Cons (f x) (fmap f xs)

  instance Applicative ListChp18 where
    pure a = ListChp18Cons a ListNil

    ListNil <*> xs      = ListNil
    xs      <*> ListNil = ListNil
    (ListChp18Cons f fs) <*> ys = (f <$> ys) `mappend` (fs <*> ys)


  instance Monad ListChp18 where
    return = pure

    ListNil >>= _              = ListNil
    (ListChp18Cons x xs) >>= f = (f x) `mappend` (xs >>= f)

  instance Arbitrary a => Arbitrary (ListChp18 a) where
    arbitrary = oneof [return ListNil, ListChp18Cons <$> arbitrary <*> arbitrary]

  instance Eq a => EqProp (ListChp18 a) where (=-=) = eq

  j :: Monad m => m (m a) -> m a
  j m = m >>= id

  l1 :: Monad m => (a -> b) -> m a -> m b
  l1 f m = m >>= (\x -> return (f x))

  l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
  l2 f m1 m2 = m1 >>= (\x -> m2 >>= (\y -> return $ f x y))

  a :: Monad m => m a -> m (a -> b) -> m b
  a m mf = m >>= (\x -> mf >>= (\f -> return $ f x))

  meh :: Monad m => [a] -> (a -> m b) -> m [b]
  meh xs mf = meh' xs mf []
      where
         meh' [] _ acc      = return acc
         meh' (x:xs) mf acc = (mf x) >>= (\b ->  meh' xs mf (acc ++ [b]))

  flipType :: Monad m => [m a] -> m [a]
  flipType xs = meh xs id

  chapter18ExerciseTests :: IO ()
  chapter18ExerciseTests = do
    quickBatch $ monoid (undefined :: Nope String)
    quickBatch $ functor (undefined :: Nope (String, String, String))
    quickBatch $ applicative (undefined :: Nope (String, String, String))
    quickBatch $ monad (undefined :: Nope (String, String, String))

    quickBatch $ monoid (undefined :: MyIdentityAgain String)
    quickBatch $ functor (undefined :: MyIdentityAgain (String, String, String))
    quickBatch $ applicative (undefined :: MyIdentityAgain (String, String, String))
    quickBatch $ monad (undefined :: MyIdentityAgain (String, String, String))

    quickBatch $ monoid (undefined :: ListChp18 String)
    quickBatch $ functor (undefined :: ListChp18 (String, String, String))
    quickBatch $ applicative (undefined :: ListChp18 (String, String, String))
    quickBatch $ monad (undefined :: ListChp18 (String, String, String))

