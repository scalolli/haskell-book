module Chapter17.ZipListMonoid where

  import Test.QuickCheck
  import Test.QuickCheck.Classes
  import Test.QuickCheck.Function
  import Test.QuickCheck.Checkers

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

  instance Eq a => EqProp (List' a) where (=-=) = eq

  instance Arbitrary a => Arbitrary (List' a) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      (elements [(Cons' x y) , Nil'])

  newtype ZipList' a = ZipList' (List' a) deriving (Eq, Show)

  instance Monoid (ZipList' a) where
    mempty = ZipList' mempty

    mappend (ZipList' Nil') (ZipList' Nil') = ZipList' Nil'
    mappend (ZipList' Nil') x = ZipList' Nil'
    mappend x (ZipList' Nil') = ZipList' Nil'
    mappend (ZipList' (Cons' a b)) (ZipList' l) = ZipList' (Cons' a (mappend b l))

  chapter17Exercises :: IO ()
  chapter17Exercises = do
    quickBatch (functor (undefined :: (List' (String, Int, Int))))
    quickBatch (monoid (undefined :: (List' String)))

