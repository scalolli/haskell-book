module Chapter16.Intermission where

  import Test.QuickCheck
  import Test.QuickCheck.Function

  a = fmap (+1) (read "[1]" :: [Int])

  b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

  c = fmap (*2) (\x -> x - 2) $ 1

  d = do
        fmap ((return '1' ++) . show) (\x -> [x, 1..3]) $ 0

--   e :: IO Integer
--   e = let ioi = readIO "1" :: IO Integer
--           changed = fmap ((read "123" :: Integer) ++ ) show ioi
--       in fmap (*3) changed

  functorIdentity :: (Functor f, (Eq (f a))) => f a -> Bool
  functorIdentity f =
        fmap id f == id f

  functorCompose :: (Functor f, (Eq (f c))) => (b -> c) -> (a -> b) -> f a -> Bool
  functorCompose f g x = ((fmap f . fmap g) x) == (fmap (f . g) x)

  f :: [Int] -> Bool
  f x = functorIdentity x

  fcompose :: [Int] -> Bool
  fcompose x = functorCompose (*2) (+1) x

  type IntInt = Fun Int Int

  fcompose' :: [Int] -> IntInt -> IntInt -> Bool
  fcompose' x (Fun _ f) (Fun _ g) = ((fmap f . fmap g) x) == (fmap (f . g) x)

  chapter16Intermission :: IO ()
  chapter16Intermission = do
    quickCheck f
    quickCheck fcompose
    quickCheck fcompose'

