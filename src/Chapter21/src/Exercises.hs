module Exercises where

  import Test.QuickCheck
  import Test.QuickCheck.Classes
  import Test.QuickCheck.Checkers
  import Data.Semigroup

  data Two a b = Two a b deriving (Eq, Show)

  instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    Two a b `mappend` Two c d = Two (a `mappend` c) (b `mappend` d)

  instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

  instance (Monoid a) => Applicative (Two a) where
    pure a = Two mempty a
    (Two a f) <*> (Two b c) = Two (a `mappend` b) (f c)

  instance Foldable (Two a) where
    foldMap f (Two a b) = f b

  instance Traversable (Two a) where
    traverse f (Two a b) = (Two a) <$> f b


  data Or a b = MyFirst a | MySecond b deriving (Eq, Show)

  instance (Monoid a, Monoid b) => Monoid (Or a b) where
    mempty = MySecond mempty
    MyFirst a `mappend` MyFirst b = MyFirst a
    MySecond a `mappend` MySecond b = MySecond $ a `mappend` b
    MySecond a `mappend` MyFirst b = MyFirst b
    MyFirst a `mappend` MySecond b = MyFirst a

  instance Functor (Or a) where
    fmap f (MyFirst a) = MyFirst a
    fmap f (MySecond b) = MySecond (f b)


  instance Monoid a => Applicative (Or a) where
    pure a = MySecond a
    (MyFirst a) <*> (MyFirst b) = MyFirst (a `mappend` b)
    (MySecond f) <*> (MySecond a) = MySecond (f a)

  instance Foldable (Or a) where
    foldMap f (MyFirst a) = mempty
    foldMap f (MySecond b) = f b

  instance Traversable (Or a) where
    traverse _ (MyFirst a) = pure (MyFirst a)
    traverse f (MySecond b) = MySecond <$> f b


  data Identity a = Identity a deriving (Eq, Ord, Show)

  instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty

    mappend (Identity a) (Identity b) = Identity (a `mappend` b)


  instance Functor Identity where
    fmap f (Identity a) = Identity (f a)


  instance Applicative Identity where
    pure = Identity

    (Identity f) <*> (Identity a) = Identity $ f a


  instance Foldable Identity where
    foldMap f (Identity a) = f a


  instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a


  instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

  instance Eq a => EqProp (Identity a) where (=-=) = eq

  instance Eq a => EqProp (Sum a) where (=-=) = eq

  identityTrigger :: Identity (Int, Int,  Maybe (Sum Int))
  identityTrigger = undefined

  chapter21Exercises :: IO ()
  chapter21Exercises = do
    quickBatch (traversable identityTrigger)


