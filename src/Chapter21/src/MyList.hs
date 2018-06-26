module MyList where

  import Data.Monoid

  data List' a = Nil' | Cons' a (List' a) deriving (Eq, Show)

  instance Monoid (List' a) where
    mempty = Nil'

    mappend x Nil'  = x
    mappend Nil' x = x
    mappend (Cons' a xs) ys = Cons' a $ xs <> ys

  instance Functor List' where
    fmap _ Nil' = Nil'
    fmap f (Cons' x y) = Cons' (f x) $ fmap f y

  instance Applicative List' where
    pure f = Cons' f Nil'
    Nil' <*> x = Nil'
    y <*> Nil' = Nil'
    (Cons' f xs) <*> ys = (f <$> ys) <> (xs <*> ys)


  instance Foldable List' where
    foldMap f (Cons' x xs) = (f x) <> (foldMap f xs)

  instance Traversable List' where
    traverse f Nil'         = pure mempty
    traverse f (Cons' x xs) = ((\x y -> Cons' x y) <$> f x <*> traverse f xs)


