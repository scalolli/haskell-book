module Chapter14.QuickCheckWithoutHspec where

  import Test.QuickCheck
  import Data.List
  import Data.Char

  main :: IO ()
  main = do
    quickCheck property_foldr
    quickCheck property_foldrConcat
--     quickCheck property_take
    quickCheck property_readShowAndBack
--     quickCheck property_square
    quickCheck property_idempotenceForCapitalizeWord
    quickCheck property_idempotenceForSorting


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


  twice :: (a -> a) -> (a -> a)
  twice f = f . f

  fourTimes :: (a -> a) -> (a -> a)
  fourTimes = twice . twice

  property_idempotenceForCapitalizeWord :: [Char] -> Bool
  property_idempotenceForCapitalizeWord x =
            (capitalizeWord x == twice capitalizeWord x) &&
                                           (capitalizeWord x == fourTimes capitalizeWord x)

  property_idempotenceForSorting :: [Int] -> Bool
  property_idempotenceForSorting x = (sort x == twice sort x) && (sort x == fourTimes sort x)

  capitalizeWord :: [Char] -> [Char]
  capitalizeWord xs = fmap toUpper xs


