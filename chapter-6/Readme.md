# Chapter Exercises

Does it typecheck? 

1. Person does not derive Show
    ```
        data Person = Person Bool 
            deriving Show
        printPerson :: Person -> IO ()
        printPerson person = putStrLn person
    ```    

No point doing them in readme anymore, all done in [ChapterExercises](ChapterExercises.hs)
