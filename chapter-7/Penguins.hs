module Penguins where

data WherePenguinsLive = 
        Galapagos 
    |   Antartica
    |   Australia
    |   SouthAfrica
    |   SouthAmerica    
    deriving (Eq, Show)

data Penguin = 
    Peng WherePenguinsLive
    deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica  _          = False 

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng location) = location

humboldt = Peng SouthAmerica
gentoo   = Peng Antartica
macaroni = Peng Antartica
little = Peng Australia
galapagos = Peng Galapagos

antarticaPenguin (Peng Antartica) = True
antarticaPenguin _                = False

galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False

galapagosOrAntartica :: Penguin -> Bool
galapagosOrAntartica penguin = 
    (antarticaPenguin penguin) || (galapagosPenguin penguin)        
