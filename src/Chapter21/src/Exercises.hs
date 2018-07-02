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


-- Constant
  newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Ord, Show)

  instance Monoid a => Monoid (Constant a b) where
    mempty = Constant mempty

    mappend (Constant a) (Constant b) = Constant (a `mappend` b)

  instance Functor (Constant a) where
    fmap f (Constant a) = Constant a

  instance Monoid a => Applicative (Constant a) where
     pure b = Constant mempty

     (Constant a) <*> (Constant b) = Constant (a `mappend` b)

  instance Foldable (Constant a) where
    foldMap f (Constant a) = mempty


  instance Traversable (Constant a) where
    traverse f (Constant a) = pure $ Constant a


  instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

  instance Eq a => EqProp (Constant a b) where (=-=) = eq

--   Maybe

  data Optional a = Nada | Yep a deriving (Eq, Show)

  instance Monoid a => Monoid (Optional a) where
    mempty = Yep mempty

    mappend (Yep a) (Yep b) = Yep (a `mappend` b)
    Nada `mappend` _        = Nada
    _    `mappend` Nada     = Nada


  instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = oneof [return Nada, Yep <$> arbitrary]

  instance Eq a => EqProp (Optional a) where (=-=) = eq

  instance Functor Optional where
    fmap f (Yep a) = Yep (f a)
    fmap f Nada    = Nada

  instance Applicative Optional where
    pure = Yep

    (Yep f) <*> (Yep a) = Yep (f a)
    Nada <*> _    = Nada
    _ <*> Nada    = Nada

  instance Foldable Optional where
    foldMap f (Yep a) = f a
    foldMap f Nada    = mempty

  instance Traversable Optional where
    traverse f (Yep a)    = Yep <$> f a
    traverse f Nada       = pure Nada

  chapter21Exercises :: IO ()
  chapter21Exercises = do
    quickBatch (traversable identityTrigger)
    quickBatch $ monoid (undefined :: (Constant String String))
    quickBatch $ functor (undefined :: Constant String (String, String, String))
    quickBatch $ applicative (undefined :: Constant String (String, String, String))
    quickBatch (traversable (undefined :: Constant String (Int, String, Maybe String)))

    quickBatch $ (monoid (undefined :: (Optional String)))
    quickBatch $ (functor (undefined :: Optional (Int, Int, String)))
    quickBatch $ (applicative (undefined :: Optional (Int, Int, String)))
    quickBatch $ (traversable (undefined :: Optional (Int, Int, Maybe String)))



