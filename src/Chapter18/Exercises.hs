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

  nopeMonadTests :: IO ()
  nopeMonadTests = do
    quickBatch $ monoid (undefined :: Nope String)
    quickBatch $ functor (undefined :: Nope (String, String, String))
    quickBatch $ applicative (undefined :: Nope (String, String, String))
    quickBatch $ monad (undefined :: Nope (String, String, String))


