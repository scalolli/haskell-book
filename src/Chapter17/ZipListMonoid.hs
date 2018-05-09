module Chapter17.ZipListMonoid where

  import Test.QuickCheck
  import Test.QuickCheck.Classes
  import Test.QuickCheck.Function
  import Test.QuickCheck.Checkers
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


  append :: List' a -> List' a -> List' a
  append Nil' ys = ys
  append (Cons' x xs) ys = Cons' x (append xs ys)

  fold :: (a -> b -> b) -> b -> List' a -> b
  fold _ b Nil' = b
  fold f b (Cons' h t) = f h (fold f b t)

  concat' :: List' (List' a) -> List' a
  concat' = fold append Nil'

  flatMap :: (a -> List' b) -> List' a -> List' b
  flatMap f xs = concat' (fmap f xs)

  chapter17Exercises :: IO ()
  chapter17Exercises = do
    quickBatch (functor (undefined :: (List' (String, Int, Int))))
    quickBatch (monoid (undefined :: (List' String)))
    quickBatch (applicative (undefined :: (List' (String, Int, Int))))

