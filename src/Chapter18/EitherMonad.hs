module Chapter18.EitherMonad where

  import Test.QuickCheck
  import Data.Semigroup
  import Test.QuickCheck.Function
  import Test.QuickCheck.Classes
  import Test.QuickCheck.Checkers

  data MySum a b = MyFirst a | MySecond b deriving (Eq, Show)

  instance (Arbitrary a, Arbitrary b) => Arbitrary (MySum a b) where
    arbitrary = do
      z <- elements [MyFirst <$> arbitrary, MySecond <$> arbitrary]
      z

  instance (Eq a, Eq b) => EqProp (MySum a b) where
    (=-=) = eq

  instance (Monoid a, Monoid b)  => Monoid (MySum a b) where
    mempty = MySecond mempty

    mappend (MyFirst a) (MyFirst b)   = MyFirst (a `mappend` b)
    mappend (MySecond a) (MyFirst b)  = MyFirst b
    mappend (MyFirst a) (MySecond b)  = MyFirst a
    mappend (MySecond a) (MySecond b) = MySecond (a `mappend` b)

  instance Functor (MySum a) where
    fmap f (MyFirst a)   = MyFirst a
    fmap f (MySecond b)  = MySecond (f b)

  instance Monoid a => Applicative (MySum a) where
    pure = MySecond

    MyFirst a <*> MyFirst b   = MyFirst (a `mappend` b)
    MySecond f <*> MySecond b = MySecond (f b)
    MyFirst a <*> MySecond b  = MyFirst a
    MySecond a <*> MyFirst b  = MyFirst b

  instance Monoid a => Monad (MySum a) where
    return = MySecond

    MySecond a  >>= f   = f a
    MyFirst a  >>= _    = MyFirst a

  eitherMonadTests :: IO ()
  eitherMonadTests = do
    quickBatch $ monoid (undefined :: MySum String String)
    quickBatch $ functor (undefined :: MySum String (String, String, String))
    quickBatch $ applicative (undefined :: MySum String (String, String, String))
    quickBatch $ monad (undefined :: MySum String (String, String, String))

--     Monad Laws
--   Right Identity   m >>= return    = m
--   Left Identity    return a >>= f  = f a
--   Composition      m >>= f >>= g   = m >>= (\x -> f x >>= g)


--   Right Identity
--         m >>= return    = m
--   Left Identity
--         return a >>= f  = f a
--   Composition
--         m >>= f >>= g   = m >>= (\x -> f x >>= g)




