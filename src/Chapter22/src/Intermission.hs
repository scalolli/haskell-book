{-# LANGUAGE InstanceSigs #-}
module Intermission where

import Data.Char
import Control.Applicative

newtype Reader r a = Reader { runReader :: r -> a }

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

ask :: Reader a a
ask = Reader id

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

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where   
    fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where   
    pure :: a -> Reader r a
    pure a = Reader (\x -> a)

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (<*>) (Reader f) (Reader g) = Reader (\r -> f r (g r))

instance Monad (Reader r) where 
    return :: a -> Reader r a
    return a = Reader (\x -> a)

    (>>=) :: Reader r a -> (a -> Reader r b) -> (Reader r b)
    (>>=) (Reader f) g = Reader $ \r -> runReader (g (f r)) $ r

foo :: (Functor f, Num a) => f a -> f a
foo f = fmap (+1) f

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot xs = bar (foo xs) xs
