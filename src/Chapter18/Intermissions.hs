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
    putStrLn "Welcome to Haskell !"   >>
    putStrLn "Please enter your name: "  >>
    getLine >>=
    (\name -> putStrLn $ "Welcome " ++ name ++ " to the wonderful world of Haskell !")

  sequencingWithDoSyntax :: IO ()
  sequencingWithDoSyntax = do
    putStrLn "Welcome to Haskell !"
    putStrLn "Please enter your name: "
    name <- getLine
    putStrLn $ "Welcome " ++ name ++ " to the wonderful world of Haskell !"

  twiceWhenEven :: [Integer] -> [Integer]
  twiceWhenEven xs = do
    x <- xs
    if(even x)
     then [x*x, x*x]
     else []

  binding :: IO ()
  binding = do
    getLine >>= putStrLn

  buildTupleUsingApplicative :: Num a => Maybe (a, a, a)
  buildTupleUsingApplicative = pure (,,) <*> Just 1 <*> Just 2 <*> Just 3

  buildTupleUsingMonad :: Num a => Maybe (a, a, a)
  buildTupleUsingMonad = do
    a <- Just 1
    b <- Just 2
    c <- Just 3
    return (a, b, c)

