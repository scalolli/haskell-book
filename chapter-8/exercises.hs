catConny :: String -> String -> String
catConny x y = x ++ " mrow " ++ y

flippy = flip catConny

appedCatty = catConny "woops"
frappe = flippy "haha"

sumOfNumbers :: (Eq a, Num a) => a -> a
sumOfNumbers 0 = 0
sumOfNumbers 1 = 1
sumOfNumbers n = n + sumOfNumbers (n-1)

integralMultiplication :: Integral a => a -> a -> a
integralMultiplication x 0 = 0
integralMultiplication 0 y = 0
integralMultiplication x y = go x x y
    where go a b count
            | count == 1 = a
            | otherwise = go (a + b) b (count-1)