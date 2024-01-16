{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module AbstractInteger where

-- Here are some definations for AbstractNatural.
-- You will probably define your AbstractInteger based on
-- AbstractNatural.

data AbstractNatural = Zero | S AbstractNatural
 deriving (Show)

-- Once we tell Haskell that AbstractNatural can do equality
-- comparisons and how AbstractNatural is totally ordered, we
-- get other functions for free, like /= and >= and > and <
--
-- You may not need these so I've left them commented out, but
-- you should understand why they work.
--
-- instance Eq AbstractNatural where
--   Zero == Zero = True
--   Zero == S _  = False
--   S _  == Zero = False
--   S x  == S y  = x == y
--
-- instance Ord AbstractNatural where
--   Zero <= Zero = True
--   Zero <= S _  = True
--   S _  <= Zero = False
--   S x  <= S y  = x <= y
--
-- successorNat :: AbstractNatural -> AbstractNatural
-- successorNat = S
--
-- predecessorNat :: AbstractNatural -> AbstractNatural
-- predecessorNat Zero  = Zero
-- predecessorNat (S x) = x


-- Figure out how you will define integers...

-- type constructors Pos, Neg (two zeros)
-- (nat, nat) (n,m) === n -m 
-- make naturals start at one-> zero, pos nat, neg nat

data AbstractInteger = Pos AbstractNatural | Neg AbstractNatural
 deriving (Show)

-- ...then fill out the functions below for your AbstractInteger type.

successor :: AbstractInteger -> AbstractInteger
successor (Neg Zero) = Pos (S Zero)
successor (Pos n) = Pos (S n)
successor (Neg (S m)) = Neg m 

predecessor :: AbstractInteger -> AbstractInteger
predecessor (Pos Zero) = Neg (S Zero)
predecessor (Pos (S n)) = Pos n
predecessor (Neg m) = Neg (S m)

-- Be sure to add type declarations to all these functions too.
negator :: AbstractInteger -> AbstractInteger 
negator (Pos n) = Neg n
negator (Neg n) = Pos n

absolute :: AbstractInteger -> AbstractInteger
absolute (Pos n) = (Pos n)
absolute (Neg n) = negator (Neg n)

add :: AbstractInteger -> AbstractInteger -> AbstractInteger
add (Pos Zero) n = n
add (Pos n) m =  add (predecessor (Pos n)) (successor m)
add (Neg n) m = add (successor (Neg n)) (predecessor m)



-- add (Pos (S n)) (Pos (S m)) = 

difference :: AbstractInteger -> AbstractInteger -> AbstractInteger
difference x y = add x (negator y)
{-
difference (Pos n) (Pos Zero) = Pos n
difference (Pos n) (Neg Zero) = Pos n
difference (Neg n) (Pos Zero) = Neg n
difference (Neg n) (Neg Zero) = Neg n
difference (Pos n) (Pos m) = difference (predecessor (Pos n)) (predecessor (Pos m))
difference (Neg n) (Pos m) = difference (predecessor (Neg n)) (predecessor (Pos m))
difference (Pos n) (Neg m) = add (Pos n) (negator (Neg m))
difference (Neg n) (Neg m) = add (Neg n) (negator (Neg m))
-}

-- I could have made this pretty and precise or i could do it quickly
multiply :: AbstractInteger -> AbstractInteger -> AbstractInteger
multiply (Pos Zero) n  = Pos Zero
multiply (Pos n) m = add (multiply (predecessor (Pos n)) m) m
multiply (Neg n) m = multiply (negator (Neg n)) (negator m)





-- To define division and modulo, you will probably need
-- comparison functions: == <= < > >=.
--
-- If you just provide == and <= below, Haskell will give
-- you the rest for free.
instance Eq AbstractInteger where
 (Pos Zero) == (Pos Zero) = True
 (Pos Zero) == n = False
 (Pos (S n)) == (Pos (S m)) = (Pos n) == (Pos m)
 (Neg n) == (Neg m) = (successor (Neg n)) == (successor (Neg m))
 _ == _ = False




    -- add more cases here...

instance Ord AbstractInteger where
 (Pos Zero) <= (Pos n) = True
 (Pos Zero) <= (Neg n) = False
 (Neg Zero) <= (Pos n) = True
 (Neg Zero) <= (Neg n) = False
 (Neg n) <= (Pos m) = True
 (Pos (S n)) <= (Pos (S m)) = (Pos n) <= (Pos m)
 (Neg n) <= (Neg m) = (successor (Neg n)) <= (successor (Neg m))
 _ <= _ = False



divide :: AbstractInteger -> AbstractInteger -> AbstractInteger
divide (Pos Zero) n = (Pos Zero)
divide (Pos n) (Pos m)
 | x==y = one 
 | x < y = Pos Zero 
 | otherwise = successor (divide (difference x y) y)
 where (x,y) = ((Pos n), (Pos m))
divide (Pos n) (Neg m)
 | absolute x == absolute y = negativeOne
 | x > negator (y) = predecessor (divide (add x y) y)
 | otherwise = Pos Zero
 where (x,y) = ((Pos n), (Neg m))
divide (Neg n) (Pos m)
 | x <= (Pos Zero) = predecessor (divide (add x y) y)
 |otherwise = Pos Zero
 where (x,y) = ((Neg n), (Pos m))
divide (Neg n) (Neg m) 
 | x/=y = successor (divide (difference x y) y)
 | otherwise = one
 where (x,y) = ((Neg n), (Neg m))



modulo :: AbstractInteger -> AbstractInteger -> AbstractInteger
modulo x y = difference x (multiply (divide x y) y)


toAbstract :: Integer -> AbstractInteger
toAbstract 0 = Pos Zero
toAbstract x 
 | x > 0 = successor (toAbstract (x-1))
 | otherwise = predecessor (toAbstract (x+1))


fromAbstract :: AbstractInteger -> Integer
fromAbstract (Pos Zero) = 0
fromAbstract (Pos n) = 1 + (fromAbstract (predecessor (Pos n)))
fromAbstract (Neg m) = (fromAbstract (successor  (Neg m))) - 1

-- Take a list of strings, calculate, and return a string result.
-- You should not need to modify this, but you may eta-reduce it if you like.
evaluateRPN :: [String] -> AbstractInteger
evaluateRPN inputList = evalRPNStack [] inputList

-- The core of the RPN caluculator, Stack -> InputList -> Output
-- You will need to provide more cases.
evalRPNStack :: [AbstractInteger] -> [String] -> AbstractInteger
evalRPNStack stack inputList =
 case (stack, inputList) of
  ( x:_,           [] )            -> x -- No more input, return top of stack.
  ( y:x:stackRest, "+":inputRest ) -> evalRPNStack (add x y        : stackRest) inputRest
  ( y:x:stackRest, "*":inputRest ) -> evalRPNStack (multiply x y   : stackRest) inputRest
  ( y:x:stackRest, "-":inputRest ) -> evalRPNStack (difference x y   : stackRest) inputRest
  ( x:stackRest, "abs":inputRest ) -> evalRPNStack (absolute x  : stackRest) inputRest
  ( y:x:stackRest, "/":inputRest ) -> evalRPNStack (divide x y  : stackRest) inputRest
  ( y:x:stackRest, "%":inputRest ) -> evalRPNStack (modulo x y  : stackRest) inputRest

  -- ...add more cases here...
  -- This last case handles numeric inputs, "0" "-2" "34" etc...
  ( _,          numStr:inputRest ) -> evalRPNStack (toAbstract (read numStr) : stack) inputRest

-- Convenience constructors. Handy for testing in ghci.
-- Define zero after you've written your definition of AbstractInteger.
-- Once you define zero you should get the rest for free.
zero  = Pos Zero
one   = successor zero
two   = successor one
three = successor two
four  = successor three
five  = successor four
six   = successor five
seven = successor six
eight = successor seven
nine  = successor eight
ten   = successor nine

negativeOne   = predecessor zero
negativeTwo   = predecessor negativeOne
negativeThree = predecessor negativeTwo
negativeFour  = predecessor negativeThree
negativeFive  = predecessor negativeFour
negativeSix   = predecessor negativeFive
negativeSeven = predecessor negativeSix
negativeEight = predecessor negativeSeven
negativeNine  = predecessor negativeEight
negativeTen   = predecessor negativeNine
