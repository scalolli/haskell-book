module Exercises where

  import Control.Monad
  import System.Exit (exitSuccess)
  import Data.Char

  palindrome :: IO ()
  palindrome = forever $ do
      putStrLn "Enter a word to check for palindrome: "
      line1 <- getLine
      case (line1 == reverse line1) of
          True ->
            putStrLn "Its a palindrome"
          False ->
            do
              putStrLn $ line1 ++ " is not a palindrome."
              exitSuccess


  palindromeWords :: IO ()
  palindromeWords = do
      line <- getLine
      putStrLn line
      exitSuccess

  alphabets = ['a'..'z'] ++ ['A'..'Z']

  isPalindrome :: String -> Bool
  isPalindrome words = sentenceWithPunctuation == reverse sentenceWithPunctuation
        where
          sentenceWithPunctuation = removePunctuation words

  removePunctuation :: String -> String
  removePunctuation w = map toLower $ foldr (\x b -> if (x `elem` alphabets) then x:b else b) [] w