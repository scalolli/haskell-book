module TupleFunctions where 

    addEmUp2 :: Num a => (a, a) -> a
    addEmUp2 (a, b) = a + b

    f :: (a, b) -> (c, d) -> ((b, d), (a, c))
    f (a,b) (c, d) = ((b, d), (a, c))

    fst3 :: (a,b,c) -> a
    fst3 (a,b,c) = a

    k (x, y) = x
    k1 = k ((4-1), 10)
    k2 = k ("three", (1 + 2))