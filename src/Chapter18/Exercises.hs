module Chapter18.Exercises where

  import Test.QuickCheck
  import Data.Semigroup
  import Test.QuickCheck.Function
  import Test.QuickCheck.Classes
  import Test.QuickCheck.Checkers

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
