module Chapter17.Intermission where

  import Control.Applicative
  import Data.Semigroup
  import Data.List (elemIndex)

  fLookup :: Int -> Maybe String
  fLookup x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]

  gLookup :: Int -> Maybe String
  gLookup y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]

  hLookup :: Int -> Maybe Int
  hLookup z = lookup z [(2, 3), (5, 6), (7, 8)]

  mLookup :: Int -> Maybe Int
  mLookup m = lookup m [(4, 10), (8, 13), (1, 9001)]

  added :: Maybe Integer
  added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

  yLookup :: Maybe Integer
  yLookup = lookup 3 $ zip [1,2,3] [4,5,6]

  zLookup :: Maybe Integer
  zLookup = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

  tupledLookup :: Maybe (Integer, Integer)
  tupledLookup = liftA2 (,) yLookup zLookup

  xMaybe :: Maybe Int
  xMaybe = elemIndex 3 [1, 2, 3, 4, 5]

  yMaybe :: Maybe Int
  yMaybe = elemIndex 4 [1, 2, 3, 4, 5]

  max' :: Int -> Int -> Int
  max' = max

  maxed :: Maybe Int
  maxed = max' <$> xMaybe <*> yMaybe


  xs = [1, 2, 3]
  ys = [4, 5, 6]

  x :: Maybe Integer
  x = lookup 3 $ zip xs ys

  y :: Maybe Integer
  y = lookup 2 $ zip xs ys

  summed :: Maybe Integer
  summed = sum <$> liftA2 (,) x y

  newtype Identity a = Identity a deriving (Eq, Show, Ord)

  instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

  instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity (f a)


  newtype Constant a b = Constant {getConstant :: a}
    deriving (Eq, Ord, Show)

  instance Functor (Constant a) where
    fmap f (Constant a) = Constant a

  instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (Constant x) <*> (Constant y) = Constant (mappend x y)





