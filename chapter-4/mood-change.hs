module MoodChange where 

data Mood = Blah | Woot deriving Show

changeMood Blah = Woot
changeMood    _ = Blah

greetIfCool :: String -> IO ()
greetIfCool coolness =
    if cool 
        then putStrLn "eyyy. Whats Shakin?"
    else
        putStrLn "pshhh.."    
    where cool = 
            coolness == "downright frosty yo"
        