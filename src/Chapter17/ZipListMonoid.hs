module Chapter17.ZipListMonoid where

  data List' a = Nil' | Cons' a (List' a) deriving (Eq, Show)

  instance Monoid (List' a) where
    mempty = Nil'
    mappend (Cons' a b) Nil'  = Cons' a b
    mappend Nil' (Cons' a b) = Cons' a b
    mappend Nil' Nil' = Nil'
    mappend (Cons' a b) l = Cons' a (mappend b l)

  instance Functor List' where
    fmap _ Nil' = Nil'
    fmap f (Cons' x y) = Cons' (f x) (fmap f y)

  instance Applicative List' where
    pure f = Cons' f Nil'
    Nil' <*> x = Nil'
    y <*> Nil' = Nil'
    a@(Cons' f l) <*> b@(Cons' x y) = (Cons' (f x) (a <*> y)) `mappend` (l <*> b)

