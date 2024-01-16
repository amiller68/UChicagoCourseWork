collatz :: Integer -> Integer
collatz 1 = 1
collatz n 
 | even n = 1 + collatz(div n 2)
 | otherwise = 1 + collatz(3*n +1)

collatz' :: Integer -> Integer
collatz' x = if x==1
 then 1
 else if even x
  then 1 + collatz'(div x 2)
  else 1 + collatz'(3*x+1)