module PhoneSimulator where

    import Data.Char
    import Data.List

    type Digit = Char
    type Presses = Int
    type Occurrences = Int

    data Key = Key {
        digit :: Digit,
     symbols :: [Digit] }
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
                presses = myFindIndex (symbols key) (toLower ch)
                keyPresses =
                    if(isUpper ch)
                     then [('*', 1)] ++ [(digit key, presses + 1)]
                      else [(digit key, presses + 1)]

    myFindIndex :: [Char] -> Char -> Int
    myFindIndex xs ch = go xs ch 0
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
    fingerTaps p = foldr (\(d, p) b -> p + b) 0 p

    pressesForMessage :: DaPhone -> [String] -> [(Digit, Presses)]
    pressesForMessage p msg = concat $ map (cellPhonesDead p) msg

    mostPopularLetter :: String -> Char
    mostPopularLetter word = letter
            where
                ((letter, occ, _):xs) = lettersSortedByOccurrences word

    lettersSortedByOccurrences :: String -> [(Digit, Occurrences, Presses)]
    lettersSortedByOccurrences word = sortedList
            where
                digitsToPresses = map (f phone) word
                usymbols = uniqueSymbols digitsToPresses
                symbolsWithOccurences = pressesForEachSymbol usymbols digitsToPresses
                sortedList = sortOccurrenceList symbolsWithOccurences

    sortOccurrences :: [(Digit, Occurrences, Presses)] -> [(Digit, Occurrences, Presses)]
    sortOccurrences = sortBy (\(d1, occ1, _) (d2, occ2, _) -> (flip compare) occ1 occ2)

    coolestLtr :: [String] -> Char
    coolestLtr words = head $ filteredList
        where
            digitsToPresses = concat $ [map (f phone) word | word <- words]
            sortedList = sortOccurrences $ pressesForEachSymbol (uniqueSymbols digitsToPresses) digitsToPresses
            filteredList = map (\(letter, _, _) -> letter) $ filter (\(d, _ , _) -> d /= ' ') sortedList

    f :: DaPhone -> Char -> (Digit, Presses)
    f phone ch = (ch, presses)
        where
            presses = fingerTaps $ reverseTaps phone ch

    uniqueSymbols :: [(Digit, Presses)] -> [Digit]
    uniqueSymbols xs = go xs []
        where
            go [] acc = acc
            go ((d, p):xs) acc =
                if(d `elem` acc)
                then go xs acc
                else go xs (d:acc)

    pressesForEachSymbol :: [Digit] -> [(Digit, Presses)] -> [(Digit, Occurrences, Presses)]
    pressesForEachSymbol digits digitsToPresses = map (reduceForEachSymbol digitsToPresses) digits

    reduceForEachSymbol :: [(Digit, Presses)] -> Digit -> (Digit, Occurrences, Presses)
    reduceForEachSymbol xs ch = foldr (\(d,p) acc@(acc_d, acc_occ, acc_p) ->
            if(d == ch)
             then (acc_d, acc_occ + 1, acc_p + p)
              else acc) (ch, 0, 0) xs





