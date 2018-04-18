module Chapter14.QuickCheckWithoutHspec where

  import Test.QuickCheck

  main :: IO ()
  main = do
    quickCheck property_foldr

  property_foldr :: [Int] -> Bool
  property_foldr xs = (foldr (:) [] xs) == (xs ++ [])



