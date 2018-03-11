module HigherOrder where

    data Employee = Coder 
        | Manager
        | Veep
        | CEO 
        deriving (Eq, Ord, Show)

    reportBoss :: Employee -> Employee -> IO ()
    reportBoss e e' = 
        putStrLn $ show e ++ 
        " is the boss of " ++ 
        show e'

    employeeRank :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
    employeeRank f e e' = 
        case f e e' of 
            GT -> reportBoss e e'
            EQ -> putStrLn "Neither employee is the boss"
            LT -> (flip reportBoss) e e'

    coderRulesCEOsDrool :: (Employee -> Employee -> Ordering)            
    coderRulesCEOsDrool Coder Coder = EQ
    coderRulesCEOsDrool Coder _     = GT 
    coderRulesCEOsDrool _ Coder     = LT
    coderRulesCEOsDrool e e'  = compare e e'

    dodgy x y = x + y * 10
    oneIsOne = dodgy 1 
    oneIsTwo = (flip dodgy) 2

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

    