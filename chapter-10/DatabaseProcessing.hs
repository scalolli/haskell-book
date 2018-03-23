module DatabaseProcessing where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
            [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123)) , DbNumber 9001
            , DbString "Hello, world!"
            , DbDate (UTCTime
                        (fromGregorian 1921 5 1)
                        (secondsToDiffTime 34123))
            ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = foldr collectUTCTime [] db

filterDate :: DatabaseItem -> Bool
filterDate (DbDate utcTime) = True
filterDate _  = False


collectUTCTime :: DatabaseItem -> [UTCTime] -> [UTCTime]
collectUTCTime (DbDate utcTime) xs = utcTime:xs
collectUTCTime _  xs = xs