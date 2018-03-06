module TypeClasses where

data Trivial = Trivial'

instance Eq Trivial where
    Trivial' == Trivial' = True

data DayOfWeek =
    Mon | Tue | Wed | Thu | Fri | Sat | Sun    

instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Wed Wed = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _     = False

data Date = 
    Date DayOfWeek Int

instance Eq Date where
    (==) (Date dayOfWeek day)
         (Date dayOfWeek' day') =
        dayOfWeek == dayOfWeek'
         && day == day'      

data Identity a = 
    Identity a

instance Eq a => Eq (Identity a) where
    (==) (Identity a)
        (Identity a') = a == a'             