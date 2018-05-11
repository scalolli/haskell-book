module Chapter17.ListApplicative
      (List'(..), listApplicative) where

  import Test.QuickCheck
  import Test.QuickCheck.Classes
  import Test.QuickCheck.Function
  import Test.QuickCheck.Checkers
  import Data.Monoid
  import Control.Applicative

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

  instance Eq a => EqProp (List' a) where (=-=) = eq

  instance Arbitrary a => Arbitrary (List' a) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      (elements [(Cons' x y) , Nil'])

  listApplicative :: IO ()
  listApplicative = do
    quickBatch (functor (undefined :: (List' (String, Int, Int))))
    quickBatch (monoid (undefined :: (List' String)))
    quickBatch (applicative (undefined :: (List' (String, Int, Int))))



