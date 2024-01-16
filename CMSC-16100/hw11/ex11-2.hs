method1 =
  do
     [(),()]
     x <- [1,2,3]
     pure x

method2 =
  do
     x <- [1,2,3]
     [(),()]
     pure x

--rewriting of method1
method3 = concat [a | a <- replicate 2 [1,2,3]]
-- rewriting of method2
method4 = [a | x <- [1,2,3], a <- replicate 2 x]
