module Division where

  import Test.Hspec
  import Test.QuickCheck

  type Numerator = Integer
  type Denominator = Integer
  type Quoteint = Integer
  type Remainder = Integer

  dividedBy :: Numerator -> Denominator -> (Quoteint, Remainder)
  dividedBy x y = go x y 0
      where go num denom acc
               |  num < denom = (acc, num)
               | otherwise = go (num - denom) denom (acc + 1)


  data DividedResult = DividedResult Integer | DivideByZero
                      deriving Show
  dividedBy' :: Numerator -> Denominator -> DividedResult
  dividedBy' x 0 = DivideByZero
  dividedBy' x y = DividedResult value
      where value
               |  x < 0 && y < 0 = result
               |  x < 0 || y < 0 = negate result
               | otherwise       = result
            result = fst (dividedBy (abs x) (abs y))


