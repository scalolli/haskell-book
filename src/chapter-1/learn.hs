module Learn where

-- my first comment in my first module

x = 7
y = 10
f = x + y

example1 = x 
  where x = 5

example2 = x * x 
  where x = 5    

example3 = x * y 
  where x = 5
        y = 6
example4 = x + 3
  where x = 3
        y = 1000

r1        = x * 3 + y 
  where x = 3
        y = 1000        

r2        = x * 5
  where y = 10
        x = 10 * 5 + y 
        
r3        = z / x + y 
  where x = 7 
        y = negate x
        z = y * 10
        
        
waxOn     = x * 5
  where z = 7
        y = z + 8
        x = y ^ 2
        
triple x = x * 3

waxOff x = triple x * x / 10