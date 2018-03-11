module Guard where

    myAbs x
        | x < 0 = (-x)
        | otherwise = x

    bloodNa x
        | x < 135 = "too low"
        | x > 145 = "too high"
        | otherwise = "just right"

    isRight :: (Num a, Eq a) => a -> a -> a -> String
    isRight x y z
        | x^2 + y^2 == z^2 = "is right angle"
        | otherwise = "not a right angle"


    dogYrs :: Integer -> Integer
    dogYrs x
        | x <= 0 = 0
        | x <= 1 = x * 15
        | x <= 2 = x * 12
        | x <= 4 = x * 8
        | otherwise = x * 6

    avgGrade :: (Fractional a, Ord a) => a -> Char
    avgGrade x
        | y >= 0.9 = 'A'
        | y >= 0.8 = 'B'
        | y >= 0.7 = 'C'
        | y >= 0.59 = 'D'
        | otherwise = 'F'
        where y = x / 100

    pal xs
        | xs == reverse xs = True
        | otherwise = False

    numbers x
        | x < 0 = -1
        | x == 0 = 0
        | x > 0 = 1

