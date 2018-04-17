sayHello :: String -> IO ()
sayHello x =
    putStrLn("Hello, " ++ x ++ "!")

triple x = x * 3

square x = x * x

areaOfCircle r = pi * square r