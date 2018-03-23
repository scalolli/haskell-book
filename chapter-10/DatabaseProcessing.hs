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

collectUTCTime :: DatabaseItem -> [UTCTime] -> [UTCTime]
collectUTCTime (DbDate utcTime) xs = utcTime:xs
collectUTCTime _  xs = xs


filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = foldr collectDbInteger [] db

collectDbInteger :: DatabaseItem -> [Integer] -> [Integer]
collectDbInteger (DbNumber num) xs = num:xs
collectDbInteger _ xs = xs


mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = (myMaximum . filterDbDate) $ db

myMaximum :: (Ord a) => [a] -> a
myMaximum [x] = x
myMaximum (x:xs) = if (x >= y) then x else y
            where y = myMaximum xs

sumDb :: [DatabaseItem] -> Integer
sumDb db = sum . filterDbNumber $ db