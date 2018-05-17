module Chapter18.Intermissions where

  import Control.Monad
  import Control.Applicative

  bind :: Monad m => (a -> m b) -> m a -> m b
  bind f ma = (join . fmap f) ma

  sequencing :: IO ()
  sequencing = do
    putStrLn "Hello, "
    putStrLn "World !"

  sequencing' :: IO ()
  sequencing' =
    putStrLn "Hello " *> putStrLn "World !"

  binding :: IO ()
  binding = do
    getLine >>= putStrLn

