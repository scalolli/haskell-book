module Chapter14.QuickCheckWithoutHspec where

  import Test.QuickCheck

  main :: IO ()
  main = do
    quickCheck property_foldr
    quickCheck property_foldrConcat
--     quickCheck property_take
    quickCheck property_readShowAndBack
    quickCheck property_square


  property_foldr :: [Int] -> Bool
  property_foldr xs = (foldr (:) [] xs) == (xs ++ [])

  property_foldrConcat :: [[Int]] -> Bool
  property_foldrConcat xs = (foldr (++) [] xs) == concat xs

  property_take :: Int -> [Int] -> Bool
  property_take n xs = (length (take n xs)) == n

  property_readShowAndBack :: Int -> Bool
  property_readShowAndBack x = (read (show x)) == x

  square :: Float -> Float
  square x = x * x

  squareIdentity :: Float -> Float
  squareIdentity = square . sqrt

  property_square :: Float -> Bool
  property_square x = (squareIdentity x) == x



