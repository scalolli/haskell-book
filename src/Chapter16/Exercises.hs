{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Chapter16.Exercises where

  import Test.QuickCheck
  import Test.QuickCheck.Function
  import Test.QuickCheck.Classes
  import Test.QuickCheck.Checkers
  import GHC.Generics (Generic)
  import Test.QuickCheck.Arbitrary.Generic

-- Functor for Sum a b
  data Sum a b = First a | Second b deriving (Eq, Show, Generic)

  instance Functor (Sum e) where
    fmap f (First a) = First a
    fmap f (Second b) = Second (f b)

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = genericArbitrary

--   instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
--     arbitrary = do
--         x <- arbitrary
--         y <- arbitrary
--         elements [First x, Second y]

  instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

-- Functor for Company a b c

  data Company a b c = DeepBlue a c | Something b deriving (Eq, Show, Generic)

  instance Functor (Company a b) where
    fmap f (DeepBlue a c) = DeepBlue a (f c)
    fmap f (Something b) = Something b

  instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Company a b c) where
    arbitrary = do
      x <- elements [DeepBlue <$> arbitrary <*> arbitrary, Something <$> arbitrary]
      x

  instance (Eq a, Eq b, Eq c) => EqProp (Company a b c) where (=-=) = eq

--   Functor More

  data More a b = L b a b | R a b a deriving (Eq, Show)

  instance Functor (More a) where
    fmap f (L b a b') = L (f b) a (f b')
    fmap f (R a b a') = R a (f b) a'


  functorIdentity :: (Functor f, (Eq (f a))) => f a -> Bool
  functorIdentity f =
        fmap id f == id f

  functorCompose :: (Functor f, (Eq (f c))) => (b -> c) -> (a -> b) -> f a -> Bool
  functorCompose f g x = ((fmap f . fmap g) x) == (fmap (f . g) x)

-- Functor instance for Quant

  data Quant a b = Finance | Desk a | Bloor b
    deriving (Eq, Show)

  instance Functor (Quant a) where
    fmap f Finance = Finance
    fmap f (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      elements [Finance, Desk x, Bloor y]

  type IntInt = Fun Int Int

  functorComposeForQuant :: IntInt -> IntInt -> Quant Int Int -> Bool
  functorComposeForQuant (Fun _ f) (Fun _ g) c = functorCompose f g c

-- Functor instance for K

  data K a b = K a deriving (Eq, Show)

  instance Functor (K a) where
    fmap _ (K a) = K a

  instance Arbitrary a => Arbitrary (K a b) where
    arbitrary = do
      x <- arbitrary
      return (K x)

  functorComposeForK :: (K Int Int) -> IntInt -> IntInt -> Bool
  functorComposeForK x (Fun _ f) (Fun _ g) = functorCompose f g x

-- Functor instance for Flip

  newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

  instance Functor (Flip K b) where
    fmap f (Flip (K a)) = Flip (K (f a))


  data Tuple a b = Tuple a b deriving (Eq, Show)

  instance Functor (Tuple a) where
    fmap f (Tuple a b) = Tuple a (f b)

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (Tuple x y)

  instance Functor (Flip Tuple a) where
    fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b

--   instance (Arbitrary (Tuple b a)) => Arbitrary (Flip Tuple a b) where
--     arbitrary = fmap Flip (arbitrary :: Gen (Tuple b a))

  instance (Arbitrary (Tuple b a)) => Arbitrary (Flip Tuple a b) where
    arbitrary = fmap Flip arbitrary

  data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

  instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

  data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

  instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut (fa)) = LiftItOut (fmap f fa)


  data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

  instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)


  data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

  instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)


  data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

  instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)


  data List a = Nil | Cons a (List a) deriving (Eq, Show)

  instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons a b) = Cons (f a) (fmap f b)


  data GoatLord a =
        NoGoat
      | OneGoat a
      | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Eq, Show)

  instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)


  data TalkToMe a = Halt | Print String a | Read { runFun :: (String -> a)}

  instance Show a => Show (TalkToMe a) where
    show Halt = "Halt"
    show (Print xs a) = "Print " ++ (show xs) ++ " " ++ (show a)
    show (Read f) = "Read: a function from String to a"

  instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print xs a) = Print xs (f a)
    fmap f (Read g) = Read (f . g)

-- way to use it and run (runFun (fmap (*2) (Read (read :: (String -> Int))))) "2"

  data Pair a b = Pair a b
  instance (Eq a, Eq b) => Eq (Pair a b) where
    (Pair w x) == (Pair y z) = (w == y) && (x == z)

  chapter16Exercises :: IO ()
  chapter16Exercises = do
    quickCheck (functorIdentity :: (Quant Int Int) -> Bool)
    quickCheck functorComposeForQuant
    quickCheck (functorIdentity :: (K Int Int) -> Bool)
    quickCheck functorComposeForK

    quickCheck (functorIdentity :: (Tuple Int Int) -> Bool)

    quickBatch (functor (undefined :: [(String, Int, Int)]))
    quickBatch (functor (undefined :: (Sum String (String, Int, Int))))

--     quickBatch (functor (undefined :: (Company String String (String, Int, Int))))




