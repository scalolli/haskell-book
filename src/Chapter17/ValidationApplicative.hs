module Chapter17.ValidationApplicative where

  import Test.QuickCheck
  import Test.QuickCheck.Classes
  import Test.QuickCheck.Function
  import Test.QuickCheck.Checkers
  import Control.Applicative
  import Data.Monoid

  data Validation e a = MyFailure e | MySuccess a deriving (Eq, Show)

  instance Functor (Validation e) where
    fmap f (MySuccess a) = MySuccess (f a)
    fmap f (MyFailure e) = MyFailure e

  instance Monoid e => Applicative (Validation e) where
    pure = MySuccess

    (MySuccess f) <*> (MySuccess a) = MySuccess (f a)
    (MySuccess f) <*> (MyFailure a) = MyFailure a
    (MyFailure f) <*> (MySuccess a) = MyFailure f
    (MyFailure x) <*> (MyFailure y) = MyFailure (x <> y)

  instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      (elements [MyFailure x, MySuccess y])

  instance (Eq a, Eq e) => EqProp (Validation e a) where
    (=-=) = eq

  validationApplicativeTests :: IO ()
  validationApplicativeTests = do
    quickBatch (functor (undefined :: Validation String (String, String, Int)))
    quickBatch (applicative (undefined :: Validation [String] (String, String, Int)))



