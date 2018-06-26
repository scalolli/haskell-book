module Exercises where

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


  data Or a b = First a | Second b deriving (Eq, Show)

  instance (Monoid a, Monoid b) => Monoid (Or a b) where
    mempty = Second mempty
    First a `mappend` First b = First a
    Second a `mappend` Second b = Second $ a `mappend` b
    Second a `mappend` First b = First b
    First a `mappend` Second b = First a

  instance Functor (Or a) where
    fmap f (First a) = First a
    fmap f (Second b) = Second (f b)


  instance Monoid a => Applicative (Or a) where
    pure a = Second a
    (First a) <*> (First b) = First (a `mappend` b)
    (Second f) <*> (Second a) = Second (f a)

  instance Foldable (Or a) where
    foldMap f (First a) = mempty
    foldMap f (Second b) = f b

  instance Traversable (Or a) where
    traverse _ (First a) = pure (First a)
    traverse f (Second b) = Second <$> f b

