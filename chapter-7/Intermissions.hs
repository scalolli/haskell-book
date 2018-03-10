module Intermissions where

bindExp :: Integer -> [Char]
bindExp x = let y = 5 in 
    "the integer was: " ++ show x 
    ++ " and y was: " ++ show y


mTh1 x y z = x * y * z
mTh2 x y = \z -> x * y * z
mTh3 x = \y -> \z -> x * y * z
mTh4 = \x -> \y -> \z -> x * y * z

f = (\n -> n + 1)

addOneIfOdd n = 
    case odd n of
    True -> f n 
    False -> n 

addFive = \x -> \y -> (if x > y then y else x) + 5

myFlip f x y = f y x

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False


newtype Username = 
    Username String

newtype AccountNumber =
    AccountNumber Integer

data User = 
    UnregisteredUser | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = 
    putStrLn "UnregisteredUser"
printUser (RegisteredUser 
            (Username name)
            (AccountNumber accountNumber)) = 
            putStrLn $ name ++ " " ++ show accountNumber