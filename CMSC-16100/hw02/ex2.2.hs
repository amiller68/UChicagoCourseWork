import Prelude hiding (product)

sumSquares = sum . map square

square :: Num n => n -> n
square x = x^2

product :: Num n => [n] -> n 
product [] = 1
product (x:xs) = x * product xs

productSquares = product . map square