module Tree where

import Control.Applicative

data Tree a = Empty | Leaf a | Tree (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty    = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Tree left a right) = Tree (fmap f left) (f a) (fmap f right)

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Tree left a right) = (f a) `mappend` (foldMap f left) `mappend` (foldMap f right)

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Tree left a right) = liftA3 Tree (traverse f left) (f a) (traverse f right)



