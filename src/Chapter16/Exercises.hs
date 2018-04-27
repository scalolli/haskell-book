{-# LANGUAGE FlexibleInstances #-}

module Chapter16.Exercises where

  data Sum a b = First a | Second b deriving (Eq, Show)

  instance Functor (Sum e) where
    fmap f (First a) = First a
    fmap f (Second b) = Second (f b)


  data Company a b c = DeepBlue a c | Something b

  instance Functor (Company a b) where
    fmap f (DeepBlue a c) = DeepBlue a (f c)
    fmap f (Something b) = Something b


  data More a b = L b a b | R a b a deriving (Eq, Show)

  instance Functor (More a) where
    fmap f (L b a b') = L (f b) a (f b')
    fmap f (R a b a') = R a (f b) a'


  data Quant a b = Finance | Desk a | Bloor b
    deriving (Eq, Show)

  instance Functor (Quant a) where
    fmap f Finance = Finance
    fmap f (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)


  data K a b = K a deriving (Eq, Show)

  instance Functor (K a) where
    fmap _ (K a) = K a

  newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

  instance Functor (Flip K b) where
    fmap f (Flip (K a)) = Flip (K (f a))


  data Tuple a b = Tuple a b deriving (Eq, Show)

  instance Functor (Flip Tuple a) where
    fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b

