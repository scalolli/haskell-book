module Intermissions where

  import Data.Foldable

  data Option a = None | Some a deriving (Eq, Show)

  instance Foldable Option where
    foldl f z None = z
    foldl f z (Some a) = f z a

    foldr f z None = z
    foldr f z (Some a) = f a z

    fold None = mempty
    fold (Some a) = a

    foldMap f None = mempty
    foldMap f (Some a) = f a