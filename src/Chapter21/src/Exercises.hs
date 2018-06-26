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



