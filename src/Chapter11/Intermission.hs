module Intermission where


data Person =
    Person {
    name :: String,
    age :: Int
    } deriving (Eq, Show)


data OperatingSystem =
      GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang =
      Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer {
        os :: OperatingSystem,
        lang :: ProgLang
    }
   deriving (Eq, Show)

    
nineToFive :: Programmer
nineToFive = Programmer {os = Mac, lang = Haskell}


allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill , Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer {os = a, lang = b} | a <- allOperatingSystems, b <- allLanguages]


data ThereYet = There Float Int Bool
        deriving (Eq, Show)

nope :: Float -> Int -> Bool -> ThereYet
nope a b c = There a b c

notYet :: Int -> Bool -> ThereYet
notYet = nope 22.5

notQuite :: Bool -> ThereYet
notQuite = notYet 10

yuss :: ThereYet
yuss = notQuite False


newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer
                 | WheatFarmer
                 | SoyabeanFarmer
                 deriving Show

data Farmer =
        Farmer Name Acres FarmerType
        deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False