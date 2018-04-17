module PersonBuilder where

  type Name = String
  type Age = Integer

  data Person = Person Name Age deriving Show

  data PersonInvalid =
        NameEmpty | AgeTooLow | PersonInvalidUnknown String
        deriving (Eq, Show)

  mkPerson :: Name -> Age -> Either PersonInvalid Person

  mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $
                    "Name was " ++ show name
                    ++ " Age was " ++ show age

  gimmePerson :: IO ()
  gimmePerson = do
      putStrLn "Enter name: "
      name <- getLine
      putStrLn "Enter age: "
      ageAsString <- getLine
      displayPerson name ((read ageAsString) :: Integer)

  displayPerson :: Name -> Integer -> IO ()
  displayPerson name age = do
        case mkPerson name age of
            Right p -> do
                putStrLn "Yay successfully constructed person!"
                putStrLn $ show p
            Left NameEmpty ->
                putStrLn "Name is empty"
            Left AgeTooLow ->
                putStrLn "Age too low"
            Left (PersonInvalidUnknown msg) ->
                putStrLn $ "Unknown error: " ++ msg





