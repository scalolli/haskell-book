**Intermission: Is it in normal Form?

Some more [Stack Overflow](https://stackoverflow.com/questions/6872898/haskell-what-is-weak-head-normal-form) and
[haskell wiki](https://wiki.haskell.org/Weak_head_normal_form)

1. [1, 2, 3, 4, 5]
    in NF and hence in WHNF

2. 1 : 2 : 3 : 4 : _
    awaiting eval, hence in WHNF

3. enumFromTo 1 10
    Hence neither WHNF or NF
    Function application, it has been applied to two of its arguments hence no in WHNF, its awaiting reduction to NF.

4. length [1, 2, 3, 4, 5]
    Neither since its a function which has been applied to its only argument.
    It can still be reduced to 5

5. sum (enumFromTo 1 10)
    Neither - fully applied function

6.  ['a'..'m'] ++ ['n'..'z']
    (++) is an in built function and has been applied to both of its arguments hence not in WHNF.
    But it can be still reduced hence not in NF

7. (_, 'b')
    The head is (,) so it is WHNF only, [more](https://stackoverflow.com/questions/46004882/is-the-expression-b-in-normal-form-in-weak-head-normal-form)
