module Chapter17.ZipListApplicative (zipListApplicative) where

  import Test.QuickCheck
  import Test.QuickCheck.Classes
  import Test.QuickCheck.Function
  import Test.QuickCheck.Checkers
  import Data.Monoid
  import Control.Applicative
  import Chapter17.ListApplicative

  newtype ZipList' a = ZipList' (List' a) deriving (Eq, Show)

  takeList' :: Int -> List' a -> List' a
  takeList' n Nil'         = Nil'
  takeList' 0 (Cons' x xs) = Nil'
  takeList' n (Cons' x xs) = Cons' x (takeList' (n-1) xs)

  mkInfiniteList :: ZipList' Integer
  mkInfiniteList = ZipList' go
    where
          go = Cons' 1 go

  instance Monoid (ZipList' a) where
    mempty = ZipList' mempty

    mappend (ZipList' Nil') (ZipList' Nil') = ZipList' Nil'
    mappend (ZipList' Nil') x = x
    mappend x (ZipList' Nil') = x
    mappend (ZipList' (Cons' a b)) (ZipList' l) = ZipList' (Cons' a (mappend b l))

  instance Functor ZipList' where
    fmap _ (ZipList' Nil') = ZipList' Nil'
    fmap f (ZipList' (Cons' h t)) = ZipList' (Cons' (f h) (fmap f t))

  instance Applicative ZipList' where
    pure f                          = ZipList' go
        where go = Cons' f go

    (ZipList' Nil') <*> xs          = ZipList' Nil'
    xs <*> (ZipList' Nil')          = ZipList' Nil'
    (ZipList' xs) <*> (ZipList' ys) = ZipList' (zipWithForList' xs ys)

  zipWithForList' :: (List' (a -> b))  -> List' a -> List' b
  zipWithForList' x Nil' = Nil'
  zipWithForList' Nil' x = Nil'
  zipWithForList' (Cons' f fs) (Cons' y ys) = Cons' (f y) (zipWithForList' fs ys)

  instance Arbitrary a => Arbitrary (ZipList' a) where
     arbitrary = do
      x <- arbitrary
      return (ZipList' x)

  instance Eq a => EqProp (ZipList' a) where

    xs =-= ys = xs' `eq` ys'
      where xs' = let (ZipList' l) = xs
                  in takeList' 3000 l
            ys' = let (ZipList' l) = ys
                  in takeList' 3000 l

  mkZipList :: [a] -> ZipList' a
  mkZipList xs = ZipList' (go xs)
      where
          go []      = Nil'
          go (y: ys) = Cons' y (go ys)


  zipListApplicative :: IO ()
  zipListApplicative = do

    quickBatch (functor (undefined :: (ZipList' (String, Int, Int))))
    quickBatch (monoid (undefined :: (ZipList' String)))
    quickBatch (applicative (undefined :: (ZipList' (String, Int, Int))))

