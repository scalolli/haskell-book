{-# LANGUAGE FlexibleContexts #-}
module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative
import Test.QuickCheck.Function


data S n a = S (n a) a deriving (Eq, Show)

instance (Monoid (f a), Monoid a) => Monoid (S f a) where
  mempty = S mempty mempty

  (S n a) `mappend` (S m b) = S (n `mappend` m) (a `mappend` b)

instance Functor n => Functor (S n) where
  fmap f (S x y) = S (fmap f x) (f y)

instance Foldable (S a) where
  foldMap f (S g a) = f a

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), EqProp a) => EqProp (S n a) where
  (S x y) =-= (S p q) = ((=-=) <$> x <*> p) .&. (y =-= q)

instance Traversable n => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*> f a

functorIdentityForS :: (Functor f, (Eq (f a))) => f a -> Bool
functorIdentityForS f =
      fmap id f == id f

type IntInt = Fun Int Int

functorCompose :: (Functor f, (Eq (f c))) => (b -> c) -> (a -> b) -> f a -> Bool
functorCompose f g x = ((fmap f . fmap g) x) == (fmap (f . g) x)

functorComposeForS :: IntInt -> IntInt -> S [] Int -> Bool
functorComposeForS (Fun _ f) (Fun _ g) c = functorCompose f g c

skiFree :: IO ()
skiFree = do
    putStrLn =<< show <$> sample' (arbitrary :: Gen (S [] Int))
    quickCheck (functorIdentityForS :: (S [] Int) -> Bool)
    quickCheck functorComposeForS
    quickBatch $ functor (undefined :: S [] (Int, Int, Int))


