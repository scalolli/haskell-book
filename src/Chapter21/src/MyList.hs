module MyList where

  import Data.Monoid
  import Test.QuickCheck
  import Test.QuickCheck.Classes
  import Test.QuickCheck.Checkers

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
    foldMap f Nil' = mempty

-- Link to explanation https://gist.github.com/scalolli/6fd86f72566fadcc7173048e2cb68fdf
  instance Traversable List' where
    traverse f Nil'         = pure mempty
    traverse f (Cons' x xs) = ((\x y -> Cons' x y) <$> f x <*> traverse f xs)


  instance Arbitrary a => Arbitrary (List' a) where
    arbitrary = do
      b <- choose (0, 10)
      xs <- (take b <$> arbitrary)
      return $ buildMyList xs

  buildMyList :: [a] -> List' a
  buildMyList []    = Nil'
  buildMyList (x:xs) = Cons' x (buildMyList xs)

  instance Eq a => EqProp (List' a) where (=-=) = eq

  testsForMyListInstances :: IO ()
  testsForMyListInstances = do
    quickBatch $ monoid (undefined :: (List' String))
    quickBatch $ functor (undefined :: List' (String, String, String))
    quickBatch $ applicative (undefined :: List' (Int, Int, Int))
    quickBatch (traversable (undefined :: List' (Int, String, Maybe String)))
