module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup a  []          = Nothing
myLookup a' ((a, b):xs) = if (a == a') then Just b else (myLookup a' xs)

xs = myLookup 3 $ zip x y

ys = myLookup 6 $ zip y z

zs = myLookup 4 $ zip x y

z' n = myLookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (a, b) = f a b

summed :: Num c => (c, c) -> c
summed = myUncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe a (Just b) = b


readerMain :: IO ()
readerMain = do
 print $
   sequenceA [Just 3, Just 2, Just 1]
 print $ sequenceA [x, y]
 print $ sequenceA [xs, ys]
 print $ summed <$> ((,) <$> xs <*> ys)
 print $ fmap summed ((,) <$> xs <*> zs)
 print $ bolt 7
 print $ fmap bolt z


