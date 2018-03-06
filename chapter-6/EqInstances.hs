data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn number) (TisAn number') = number == number'


data TwoIntegers =
    Two Integer Integer

instance Eq TwoIntegers where 
    (==) (Two num1 num2) (Two num1' num2') = 
        num1 == num1' && num2 == num2'

data StringOrInt = 
    TisAnInt Int | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt num) (TisAnInt num') = 
        num == num'
    (==) (TisAString str) (TisAString str') = 
        str == str'
    (==) _ _ = False       

data Pair a = 
    Pair a a 

instance Eq a => Eq (Pair a) where         
    (==) (Pair x y) (Pair x' y') = x == x' && y == y'
        