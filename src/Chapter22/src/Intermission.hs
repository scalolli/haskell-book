{-# LANGUAGE InstanceSigs #-}
module Intermission where

import Data.Char
import Control.Applicative

newtype MyReader r a = MyReader { runMyReader :: r -> a }

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tuppled :: [Char] -> ([Char], [Char])
tuppled = liftA2 (,) rev cap

ask :: MyReader a a
ask = MyReader id

newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person {humanName:: HumanName, dogName:: DogName, address:: Address} deriving (Eq, Show)

data Dog = Dog {dogsName:: DogName, dogsAddress:: Address} deriving (Eq, Show)

pers :: Person 
pers = Person (HumanName "Big Bird")
              (DogName "Barkley")
              (Address "Blah Street")

getDog :: Person -> Dog
getDog = Dog <$> dogName <*> address              

myLiftA2 :: Applicative f => 
    (a -> b -> c)
    -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> MyReader r a
asks f = MyReader f

instance Functor (MyReader r) where   
    fmap f (MyReader g) = MyReader (f . g)

instance Applicative (MyReader r) where   
    pure :: a -> MyReader r a
    pure a = MyReader (\x -> a)

    (<*>) :: MyReader r (a -> b) -> MyReader r a -> MyReader r b
    (<*>) (MyReader f) (MyReader g) = MyReader (\r -> f r (g r))

instance Monad (MyReader r) where 
    return :: a -> MyReader r a
    return a = MyReader (\x -> a)

    (>>=) :: MyReader r a -> (a -> MyReader r b) -> (MyReader r b)
    (>>=) (MyReader f) g = MyReader $ \r -> runMyReader (g (f r)) $ r

foo :: (Functor f, Num a) => f a -> f a
foo f = fmap (+1) f

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot xs = bar (foo xs) xs

flip :: (a -> b -> c) -> b -> a -> c
flip f a b = f b a