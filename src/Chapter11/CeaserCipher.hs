module Chapter11.CeaserCipher where

    import Data.Char

    ys = "ALLY"

    encrypt :: String -> String
    encrypt xs = shiftChars xs ys []
        where ys = mapChars xs [] 0     
    
    mapChars :: String -> String -> Int -> String
    mapChars [] acc _ = acc
    mapChars (x:xs) acc index        
        | x == ' ' = mapChars xs (acc ++ [x]) index
        | otherwise = mapChars xs result (index + 1)
            where result = acc ++ [z]
                  z = ys !! (index `mod` (length ys))

    shiftChars :: String -> String -> String -> String
    shiftChars [] _ acc = acc
    shiftChars (x:xs) (y:ys) acc = 
        shiftChars xs ys (acc ++ [z])
            where z = shiftChar x y

    shiftChar :: Char -> Char -> Char
    shiftChar ' ' _ = ' '
    shiftChar a b = shift (abs ((ord 'A') - (ord b))) a

    shift :: Int -> Char -> Char
    shift numOfChars x
        | isLower x = chr $ ord 'a' + (mod (ord x - ord 'a' + numOfChars) 26)
        | otherwise = chr $ ord 'A' + (mod (ord x - ord 'A' + numOfChars) 26)


    main :: IO ()
    main = do
      putStrLn "Enter word to encode: "
      word <- getLine
      putStrLn $ "Ceaser cipher is: " ++ encrypt word

    