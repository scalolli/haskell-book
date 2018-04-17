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
        

data Tuple a b = 
    Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where  
    (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'


data Which a = 
    ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where 
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'  
    (==)        _     _           = False    


data EitherOr a b =
    Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where    
    (==) (Hello a) (Hello a') = a == a'
    (==) (Goodbye a) (Goodbye a') = a == a'
    (==)        _     _           = False