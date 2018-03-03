# Chapter 5 Exercises

## TypeInference Exercises

1. The type is obvious

    ```
      *TypeInference> :t myConcatDash
      myConcatDash :: [Char] -> [Char]
    ```

2. Division / needs a fractional and by default numbers are polymorphic constants
In the below case x ends up being a Fractional hence the below type

      ```
      :t \x -> (x/3) * 5
      \x -> (x/3) * 5 :: Fractional a => a -> a
      ```
  
3. Its obvious because we alreay know its [Char]

      ```
      :t \x -> take x "hey you"
      \x -> take x "hey you" :: Int -> [Char]
      ```

4. In the below case length returns an Int hence we will need a more specific type for comparison so type of x becomes Int

      ```
      *TypeInference> :t \x -> x > (length [1..10])
      \x -> x > (length [1..10]) :: Int -> Bool
      ```
  
5. Simple we already know one parameter is a Char hence the type `Char -> Bool`
  
      ```
      *TypeInference> :t \x -> x < 'z'
      \x -> x < 'z' :: Char -> Bool
      ```
      
## Chapter Exercises

# Multiple Choice

1. A value of type `[a]` is a lost whose elements are all of some type a
2. A function of type `[[a]] -> [a]` could take a list of strings as an argument
3. A function of type `[a] -> Int -> a` returns one element of type a
4. A function of type `(a, b) -> a` take tuple as argument and returns the first value

# Determining the type

1. Determine the value and the type returned
    * `(* 9) 6` returns `54` and the type is `Num a => a`
    * `head [(0,"doge"),(1,"kitteh")]` returns `(0, "doge")` with type `Num a => (a, [Char])`
    * `head [(0 :: Integer ,"doge"),(1,"kitteh")]` returns `(0, "doge")` with type `(Integer, [Char])`
    * `if False then True else False` returns `False` and type is `Bool`
    * `length [1, 2, 3, 4, 5]` returns `5` and the type is `Int` since length returns an Int
    * `(length [1, 2, 3, 4]) > (length "TACOCAT")` returns `False` and type is `Bool`

2. WIP..
