module RandomHacking.SumTypes where

--   https://medium.com/@willkurt/why-sum-types-matter-in-haskell-ba2c1ab4e372

  type FirstName = String
  type LastName  = String
  type MiddleName = String

  data Name =   Name FirstName LastName
              | NameWithMiddle FirstName MiddleName LastName
              | TwoInitialsWithLast Char Char LastName
  data Creator = AuthorCreator Author | ArtistCreator Artist
  data Author = Author Name
  data Artist = Person Name | Band String

  data Book = Book
                { author :: Creator
                , isbn :: String
                , bookTitle :: String
                , bookYear :: Int
                , bookPrice :: Double}

  data VinylRecord = VinylRecord
                      { artist :: Creator
                      , recordTitle :: String
                      , recordYear :: Int
                      , recordPrice :: Double}

  data CollectibleToy = CollectibleToy
                        { name :: String
                        , description :: String
                        , toyPrice :: Double}

  data StoreItem =  BookItem Book
                  | RecordItem VinylRecord
                  | ToyItem CollectibleToy

  hpLoveCraft :: Creator
  hpLoveCraft = AuthorCreator (Author (TwoInitialsWithLast 'H' 'P' "LoveCraft"))

  getPrice :: StoreItem -> Double
  getPrice (BookItem x) = bookPrice x
  getPrice (RecordItem v) = recordPrice v
  getPrice (ToyItem c) = toyPrice c

  getBookPrice = getPrice (BookItem (Book hpLoveCraft "123" "Haskell" 1991 123.12))

  fCompose :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
  fCompose f xs = (fmap . fmap) f xs

