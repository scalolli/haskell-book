module Chapter9.Cipher where

  import Data.Char

  encode :: [Char] -> [Char]
  encode xs = map rshift xs

  shift :: Int -> Char -> Char
  shift numOfChars x
      | isLower x = chr $ ord 'a' + (mod (ord x - ord 'a' + numOfChars) 26)
      | otherwise = chr $ ord 'A' + (mod (ord x - ord 'A' + numOfChars) 26)

  rshift = shift 3
  lshift = shift (-3)

  decode :: [Char] -> [Char]
  decode xs = map lshift xs

  main :: IO ()
  main = do
    putStrLn "Enter word to encode: "
    word <- getLine
    putStrLn $ "Ceaser cipher is: " ++ encode word
