module PhoneSimulator where

    import Data.Char

    type Digit = Char
    type Symbol = Char
    type Presses = Int

    data Key = Key {
        digit :: Digit,
     symbols :: [Symbol] }
     deriving (Eq, Show)
    data DaPhone = DaPhone [Key] deriving (Eq, Show)

    convo :: [String]
    convo = ["Wanna play 20 questions",
               "Ya",
               "U 1st haha",
               "Lol ok. Have u ever tasted alcohol",
               "Lol ya",
               "Wow ur cool haha. Ur turn",
               "Ok. Do u think I am pretty Lol",
               "Lol ya",
               "Just making sure rofl ur turn"]

    keys :: [Key]
    keys = [Key '1' "1", Key '2' "abc2", Key '3' "def3", Key '4' "ghi4", Key '5' "jkl5",
            Key '6' "mno6", Key '7' "pqrs7", Key '8' "tuv8", Key '9' "wxyz9", Key '*' "^ *",
            Key '0' "+_0", Key '#' ".,#"]

    phone :: DaPhone
    phone = DaPhone keys

    reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
    reverseTaps (DaPhone keys) ch = keyPresses
            where
                key = findKey keys (toLower ch)
                presses = findIndex (symbols key) (toLower ch)
                keyPresses =
                    if(isUpper ch)
                     then [('*', 1)] ++ [(digit key, presses + 1)]
                      else [(digit key, presses + 1)]

    findIndex :: [Char] -> Char -> Int
    findIndex xs ch = go xs ch 0
        where
            go [] char index = index
            go (y:ys) char index =
                if(y == char)
                 then index
                  else go ys char (index + 1)

    findKey :: [Key] -> Char -> Key
    findKey (key@(Key digit symbols):xs) ch =
            if(ch `elem` symbols)
              then key
              else findKey xs ch

    cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
    cellPhonesDead p x = concat $ map (reverseTaps p) x

    fingerTaps :: [(Digit, Presses)] -> Presses
    fingerTaps p = foldr (\a b -> (snd a) + b) 0 p

    pressesForMessage :: DaPhone -> [String] -> [(Digit, Presses)]
    pressesForMessage p msg = concat $ map (cellPhonesDead p) msg





