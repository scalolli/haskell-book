module Chapter17.Exercises where

  import Data.Monoid

  pureArr :: a -> [a]
  pureArr a = [a]

  apArr :: [(a -> b)] -> [a] -> [b]
  apArr = (<*>)

  pureIO :: a -> IO a
  pureIO = return

  appIO :: IO (a -> b) -> IO a -> IO b
  appIO = (<*>)

--   appIO (return (++ " World !")) getLine

  pureTuple :: Num b => a -> (Sum b, a)
  pureTuple a = (mempty, a)


  appTuple :: Num b => (Sum b, (a -> b)) -> (Sum b, a) -> (Sum b, b)
  appTuple (a, f) (b, x) = (a <> b, f x)

  pureFn :: b -> (a -> b)
  pureFn = pure

  appFn :: (a -> (e -> c)) -> (a -> e) -> (a -> c)
  appFn = (<*>)