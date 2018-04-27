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


