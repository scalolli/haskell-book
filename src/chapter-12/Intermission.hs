module Intermission where

    ifEvenAdd2 :: Integer -> Maybe Integer
    ifEvenAdd2 x =
        if (even x)
            then Just (x + 2)
            else Nothing


    data PersonInValid = NameEmpty | AgeTooLow deriving (Eq, Show)

    type Name = String
    type Age = Integer
    type ValidatePerson a = Either [PersonInValid] a

    data Person = Person Name Age deriving (Eq, Show)


    ageOkay :: Age -> Either [PersonInValid] Age
    ageOkay age =
            case age <= 0 of
                True -> Left [AgeTooLow]
                False -> Right age


    nameOkay :: Name -> Either [PersonInValid] Name
    nameOkay name =
            case name /= "" of
                True -> Right name
                False -> Left [NameEmpty]

    mkPerson :: Name -> Age -> ValidatePerson Person
    mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

    mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
    mkPerson' (Left [NameEmpty]) (Left [AgeTooLow]) = Left [NameEmpty, AgeTooLow]
    mkPerson' (Left [NameEmpty]) (Right _) = Left [NameEmpty]
    mkPerson' (Right _) (Left [AgeTooLow]) = Left [AgeTooLow]
    mkPerson' (Right name) (Right age) = Right (Person name age)

